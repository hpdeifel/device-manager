{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DBus.UDisks
       ( Device(..)
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

import DBus
import DBus.Client
import DBus.DBusAbstraction

data Device = Device {
  objectPath    :: DBus.ObjectPath,
  mountPoints   :: Maybe [String],
  deviceFile    :: String,
  name          :: String,
  internal      :: Bool,
  hasPartitions :: Bool
} deriving (Show)

data UDisksConnection = UDCon {
  dbusCon :: Client,
  devList :: MVar [ObjectPath]
}

udisksConnect :: IO UDisksConnection
udisksConnect = connectSystem >>= udisksConnect'

udisksDisconnect :: UDisksConnection -> IO ()
udisksDisconnect (UDCon client _) = disconnect client

udisksConnect' :: Client -> IO UDisksConnection
udisksConnect' con = do
  var <- newEmptyMVar
  let ucon = UDCon con var

  devs <- devicePathList ucon
  putMVar var devs

  return ucon


getDeviceList :: UDisksConnection -> IO [Device]
getDeviceList con@(UDCon _ var) = do
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
listenEvents :: UDisksConnection -> Chan UDiskMessage -> IO ()
listenEvents con@(UDCon client _) chan = do
  mapM_ listenTo ["DeviceAdded" ,"DeviceRemoved"]
  where listenTo mem =
          listen client (matchSignal con mem) (listenCallback con chan)

listenDevice :: UDisksConnection -> Device -> Chan UDiskMessage -> IO ()
listenDevice con@(UDCon client _) dev chan = do
  listen client (matchSignal dev "Changed")
    (devListenCallback con chan)

-- Actions
----------

doMount :: UDisksConnection -> Device -> IO ()
doMount (UDCon client _) dev = do 
  _ <- invoke client dev "FilesystemMount" [toVariant ("" :: String), toVariant ([] :: [String])]
  return ()

doUnmount :: UDisksConnection -> Device -> IO ()
doUnmount (UDCon client _) dev = do
  _ <- invoke client dev "FilesystemUnmount" [toVariant ([] :: [String])]
  return ()

-- Helpers
----------

devicePathList :: UDisksConnection -> IO [ObjectPath]
devicePathList con@(UDCon client _) = do
  reply <- invoke client con "EnumerateDevices" []
  return $ fromJust (fromVariant reply)

fillInDevice :: UDisksConnection -> ObjectPath -> IO Device
fillInDevice (UDCon client _) path = do
  mount      <- prop "DeviceMountPaths"
  devFile    <- prop "DeviceFile"
  isMounted  <- prop "DeviceIsMounted"
  label      <- prop "IdLabel"
  isIntern   <- prop "DeviceIsSystemInternal"
  partitions <- prop "DeviceIsPartitionTable"

  let realMount = if isMounted then Just mount else Nothing

  return $ dev { mountPoints   = realMount
               , deviceFile    = devFile
               , name          = label
               , internal      = isIntern
               , hasPartitions = partitions
               }

  where prop x = fromVariant' <$> getProperty client dev x
        dev    = Device path Nothing "" "" True False

listenCallback :: UDisksConnection -> Chan UDiskMessage -> Signal -> IO ()
listenCallback con@(UDCon _ var) chan _ = do
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

devListenCallback :: UDisksConnection -> Chan UDiskMessage -> Signal -> IO ()
devListenCallback con@(UDCon _ var) chan sig = do
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

instance DBusObject UDisksConnection where
  getObjectPath  _ = "/org/freedesktop/UDisks"
  getInterface   _ = "org.freedesktop.UDisks"
  getDestination _ = "org.freedesktop.UDisks"
