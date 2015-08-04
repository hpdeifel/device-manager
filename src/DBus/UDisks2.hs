{-# LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module DBus.UDisks2
       ( connect
       , disconnect
       , Connection
       , getDeviceList
       , module T
       ) where

import DBus.UDisks.Types as T
import DBus.DBusAbstraction

import Control.Monad

import qualified DBus.Client as DBus
import qualified DBus.Introspection as DBus
import qualified DBus
import DBus.Client (Client)

-- Types

data Connection = Con {
  conClient :: Client
}

connect :: IO Connection
connect = do
  client <- DBus.connectSystem
  let con = Con {
        conClient = client
        }

  return con

disconnect :: Connection -> IO ()
disconnect = DBus.disconnect . conClient


getDeviceList :: Connection -> IO [BlockDevice]
getDeviceList con = do
  paths <- getDevicePaths con
  forM paths $ \p -> do
    props <- getAllProperties (conClient con)  p BlockDeviceIface
    -- Let's see if it implements the FileSystem interface
    ifaces <- getInterfaces (conClient con) p
    fsProps <- if fsIface `elem` ifaces
               then Just <$> getAllProperties (conClient con) p fsIface
               else return Nothing
    case propertiesToBlockDevice props fsProps of
      Left err -> error err -- FIXME: return either
      Right res -> return res

  where fsIface = "org.freedesktop.UDisks2.Filesystem"

getDevicePaths :: Connection -> IO [JustAPath]
getDevicePaths con = do
  obj <- introspect (conClient con) DevicePathList
  return $ map (mkPathObj . DBus.objectPath) (DBus.objectChildren obj)


data DevicePathList = DevicePathList

instance DBusObject DevicePathList where
  type DefaultInterface DevicePathList = Introspectable
  getObjectPath _ =  "/org/freedesktop/UDisks2/block_devices"
  getDestination _ = "org.freedesktop.UDisks2"

instance Implements DevicePathList Introspectable

mkPathObj :: DBus.ObjectPath -> JustAPath
mkPathObj = JustAPath "org.freedesktop.UDisks2"

data BlockDeviceIface = BlockDeviceIface
instance DBusInterface BlockDeviceIface where
  getInterface _ = "org.freedesktop.UDisks2.Block"
