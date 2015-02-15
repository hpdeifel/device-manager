{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}

module DBus.UDisks
       ( Device(..)
       , isMounted
       , ObjectPath
       , UDisksConnection
       , udisksConnect
       , udisksConnect'
       , udisksDisconnect
       , getDeviceList

       , UDiskMessage(..)
       , listenEvents
       , listenDevice

       , doMount
       , doUnmount
       ) where

import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Data.Set as S
import Data.Set ((\\))
import qualified Data.Text as T

import DBus
import DBus.Client
import DBus.DBusAbstraction
import ErrorLogger

data Device = Device {
  objectPath    :: DBus.ObjectPath,
  mountPoints   :: Maybe [String],
  deviceFile    :: String,
  name          :: String,
  vendor        :: String,
  model         :: String,
  internal      :: Bool,
  hasPartitions :: Bool,
  hasMedia      :: Bool
} deriving (Show)

isMounted :: Device -> Bool
isMounted = isJust . mountPoints

data UDisksConnection a = UDCon {
  dbusCon :: Client,
  devList :: MVar [ObjectPath],
  errLog  :: a
}

udisksConnect :: (ErrorLogger log) => log -> IO (UDisksConnection log)
udisksConnect logger = connectSystem >>= udisksConnect' logger

udisksDisconnect :: (UDisksConnection a) -> IO ()
udisksDisconnect (UDCon client _ _) = disconnect client

udisksConnect' :: ErrorLogger log => log -> Client -> IO (UDisksConnection log)
udisksConnect' logger con = do
  var <- newEmptyMVar
  let ucon = UDCon con var logger

  devs <- devicePathList ucon
  putMVar var devs

  return ucon


getDeviceList :: (UDisksConnection a) -> IO [Device]
getDeviceList con@(UDCon _ var _) = do
  list <- takeMVar var
  devs <- mapM (fillInDevice con) list
  putMVar var list
  return devs

-- Asynchronous listener
------------------------

data UDiskMessage = DeviceAdded Device
                  | DeviceRemoved ObjectPath
                  | DeviceChanged Device

-- | Do not call twice
listenEvents :: ErrorLogger a => (UDisksConnection a) -> Chan UDiskMessage -> IO ()
listenEvents con@(UDCon client _ _) chan = do
  mapM_ listenTo ["DeviceAdded" ,"DeviceRemoved"]
  where listenTo mem =
          listen client (matchSignal con mem) (listenCallback con chan)

listenDevice :: (UDisksConnection a) -> Device -> Chan UDiskMessage -> IO ()
listenDevice con@(UDCon client _ _) dev chan = do
  listen client (matchSignal dev "Changed")
    (devListenCallback con chan)

-- Actions
----------

doMount :: ErrorLogger a => UDisksConnection a -> Device -> IO ()
doMount (UDCon client _ logger) dev =
  invoke client dev "FilesystemMount" [toVariant ("" :: String), toVariant ([] :: [String])] >>= \case
    Left err -> logError logger (T.pack err)
    Right _  -> return ()

doUnmount :: ErrorLogger a => UDisksConnection a -> Device -> IO ()
doUnmount (UDCon client _ logger) dev =
  invoke client dev "FilesystemUnmount" [toVariant ([] :: [String])] >>= \case
    Left err -> logError logger (T.pack err)
    Right _ -> return ()

-- Helpers
----------

devicePathList :: ErrorLogger a => (UDisksConnection a) -> IO [ObjectPath]
devicePathList con@(UDCon client _ logger) = do
  reply <- invoke client con "EnumerateDevices" []
  case reply of
    Left err -> logError logger (T.pack err) >> return []
    Right var -> return $ fromJust (fromVariant var)

fillInDevice :: (UDisksConnection a) -> ObjectPath -> IO Device
fillInDevice (UDCon client _ _) path = do
  mount      <- prop "DeviceMountPaths"
  devFile    <- prop "DeviceFile"
  isMounted  <- prop "DeviceIsMounted"
  label      <- prop "IdLabel"
  vendor'    <- prop "DriveVendor"
  model'     <- prop "DriveModel"
  isIntern   <- prop "DeviceIsSystemInternal"
  partitions <- prop "DeviceIsPartitionTable"
  media      <- prop "DeviceIsMediaAvailable"

  let realMount = if isMounted then Just mount else Nothing

  return $ dev { mountPoints   = realMount
               , deviceFile    = devFile
               , name          = label
               , vendor        = vendor'
               , model         = model'
               , internal      = isIntern
               , hasPartitions = partitions
               , hasMedia      = media
               }

  where prop x = fromVariant' <$> getProperty client dev x
        dev    = Device path Nothing "" "" "" "" True False False

listenCallback :: ErrorLogger a => (UDisksConnection a) -> Chan UDiskMessage -> Signal -> IO ()
listenCallback con@(UDCon _ var _) chan _ = do
  oldDevices <- takeMVar var
  newDevices <- devicePathList con

  let oldSet = S.fromList oldDevices
      newSet = S.fromList newDevices
      added  = newSet \\ oldSet
      deleted = oldSet \\ newSet

  forM_ (S.toList added) $ \d -> do
    dev <- fillInDevice con d
    writeChan chan (DeviceAdded dev)

  forM_ (S.toList deleted) $ \d -> do
    writeChan chan (DeviceRemoved d)

  putMVar var newDevices

devListenCallback :: (UDisksConnection a) -> Chan UDiskMessage -> Signal -> IO ()
devListenCallback con@(UDCon _ var _) chan sig = do
  list <- takeMVar var
  dev <- fillInDevice con (signalPath sig)
  writeChan chan (DeviceChanged dev)
  putMVar var list

-- DBus Helper stuff
--------------------

instance DBusObject Device where
  getObjectPath  dev = objectPath dev
  getInterface   _   = "org.freedesktop.UDisks.Device"
  getDestination _   = "org.freedesktop.UDisks"

instance DBusObject (UDisksConnection a) where
  getObjectPath  _ = "/org/freedesktop/UDisks"
  getInterface   _ = "org.freedesktop.UDisks"
  getDestination _ = "org.freedesktop.UDisks"
