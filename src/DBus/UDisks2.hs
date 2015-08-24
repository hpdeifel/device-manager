{-# LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module DBus.UDisks2
       ( connect
       , disconnect
       , withConnection
       , Connection
       , nextEvent
       ) where

import DBus.UDisks.Types as T
import DBus.DBusAbstraction

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Exception

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as M

import qualified DBus.Client as DBus
import qualified DBus
import DBus.Client (Client)

import Control.Concurrent.STM

-- Types

data Event = InterfaceAdded | InterfaceRemoved | PropertyChanged
           deriving (Show)

data Connection = Con {
  conClient :: Client,
  conObjectMap :: TMVar ObjectMap,
  conEventQueue :: TQueue Event,
  conSigHandler :: DBus.SignalHandler
}

connect :: IO (Either Text (Connection, ObjectMap))
connect = runExceptT $ do
  client <- lift DBus.connectSystem
  var    <- lift newEmptyTMVarIO
  events <- lift newTQueueIO

  sigHandler <- lift $ connectSignals client var events

  let cleanup e = do
        -- If the initial Objects can't be retrieved, but signal handlers have
        -- already been called, those would block forever waiting for the
        -- objMap. Thus we give them an empty one.
        lift $ atomically $ putTMVar var M.empty
        throwE e

  objMap <- getInitialObjects client
            `catchE` cleanup

  lift $ atomically $ putTMVar var objMap

  let con =  Con { conClient = client
                 , conObjectMap = var
                 , conEventQueue = events
                 , conSigHandler = sigHandler
                 }

  return (con, objMap)

disconnect :: Connection -> IO ()
disconnect con = do
  DBus.removeMatch (conClient con) (conSigHandler con)
  DBus.disconnect $ conClient con

withConnection :: ((Connection, ObjectMap) -> IO a)
               -> IO (Either Text a)
withConnection body = bracket connect (traverse $ disconnect .fst)
  (traverse body)

nextEvent :: Connection -> IO Event
nextEvent = atomically . readTQueue . conEventQueue

connectSignals :: Client -> TMVar ObjectMap -> TQueue Event
               -> IO DBus.SignalHandler
connectSignals client var queue = listenWild client base handler
  where base = "/org/freedesktop/UDisks2"
        handler = signalHandler client var queue

getInitialObjects :: Client -> ExceptT Text IO ObjectMap
getInitialObjects client =
  lift (invoke client ObjectManager "GetManagedObjects" []) >>= \case
    Left e -> throwE (Text.pack $ show e)
    Right m -> ExceptT $ return $ runExcept $ parseObjectMap $ fromVariant' m

signalHandler :: Client -> TMVar ObjectMap -> TQueue Event -> DBus.Signal -> IO ()
signalHandler client var event sig
  | DBus.signalMember sig == "InterfaceAdded" = return ()
  | DBus.signalMember sig == "InterfaceRemoved" = return ()
  | otherwise = return ()

data ObjectManager = ObjectManager

instance DBusObject ObjectManager where
  type DefaultInterface ObjectManager = ObjectManager
  getObjectPath _ = "/org/freedesktop/UDisks2"
  getDestination _ = "org.freedesktop.UDisks2"

instance DBusInterface ObjectManager where
  getInterface _ = "org.freedesktop.DBus.ObjectManager"

instance Implements ObjectManager ObjectManager
