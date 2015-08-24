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

import Debug.Trace

-- Types

data Event = ObjectAdded DBus.ObjectPath Object
           | ObjectRemoved DBus.ObjectPath
           | ObjectChanged DBus.ObjectPath Object
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
signalHandler _ var events sig
  | member == "InterfacesAdded" = handleAdded var events path props
  | member == "InterfacesRemoved" = return ()
  | otherwise = return ()

  where member = DBus.signalMember sig
        args = DBus.signalBody sig
        path = fromVariant' $ args !! 0
        props = fromVariant' $ args !! 1

handleAdded :: TMVar ObjectMap -> TQueue Event -> DBus.ObjectPath -> InterfaceMap -> IO ()
handleAdded objMapVar events path ifaces = atomically $ do
  objMap <- takeTMVar objMapVar

  putTMVar objMapVar =<< case M.lookup path objMap of

    -- Object not yet present, must be new
    Nothing -> case runExcept $ parseObject path ifaces of
      Left _ -> return objMap -- TODO Handle error (maybe log it)
      Right newObj -> do
        writeTQueue events $ ObjectAdded path newObj
        return $ M.insert path newObj objMap

    -- Object present, modify it
    Just oldObj -> case runExcept $ addInterfaces oldObj ifaces of
      Left e -> traceShow e $ return objMap -- TODO Handle error (maybe log it)
      Right newObj -> do
        writeTQueue events $ ObjectChanged path newObj
        return $ M.insert path newObj objMap

  return ()

data ObjectManager = ObjectManager

instance DBusObject ObjectManager where
  type DefaultInterface ObjectManager = ObjectManager
  getObjectPath _ = "/org/freedesktop/UDisks2"
  getDestination _ = "org.freedesktop.UDisks2"

instance DBusInterface ObjectManager where
  getInterface _ = "org.freedesktop.DBus.ObjectManager"

instance Implements ObjectManager ObjectManager
