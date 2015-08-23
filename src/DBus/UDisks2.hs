{-# LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module DBus.UDisks2
       ( connect
       , disconnect
       , withConnection
       , Connection
       , getInitialObjects
       , Event(..)
       , connectSignals
       , module T
       ) where

import DBus.UDisks.Types as T
import DBus.DBusAbstraction

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Exception

import Data.Text (Text)
import qualified Data.Text as Text

import qualified DBus.Client as DBus
import qualified DBus
import DBus.Client (Client)

-- Types

data Connection = Con {
  conClient :: Client
}

newtype DeviceId = DeviceId DBus.ObjectPath
                 deriving (Show)

data Event = DeviceAdded BlockDevice
                 | DeviceChanged BlockDevice
                 | DeviceRemoved DeviceId

connect :: IO Connection
connect = do
  client <- DBus.connectSystem
  let con = Con
            { conClient = client
            }

  return con

disconnect :: Connection -> IO ()
disconnect = DBus.disconnect . conClient

withConnection :: (Connection -> IO ()) -> IO ()
withConnection = bracket connect disconnect

connectSignals :: Connection -> IO ()
connectSignals con = void $ listenWild (conClient con) base print
  where base = "/org/freedesktop/UDisks2"

getInitialObjects :: Connection -> ExceptT Text IO ObjectMap
getInitialObjects con =
  lift (invoke (conClient con) ObjectManager "GetManagedObjects" []) >>= \case
    Left e -> throwE (Text.pack $ show e)
    Right m -> ExceptT $ return $ runExcept $ parseObjectMap $ fromVariant' m

data ObjectManager = ObjectManager

instance DBusObject ObjectManager where
  type DefaultInterface ObjectManager = ObjectManager
  getObjectPath _ = "/org/freedesktop/UDisks2"
  getDestination _ = "org.freedesktop.UDisks2"

instance DBusInterface ObjectManager where
  getInterface _ = "org.freedesktop.DBus.ObjectManager"

instance Implements ObjectManager ObjectManager
