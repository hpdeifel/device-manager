{-# LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module DBus.UDisks2
       ( connect
       , disconnect
       , withConnection
       , Connection
       , nextEvent
       ) where

import DBus.UDisks2.Types as T
import DBus.DBusAbstraction

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Exception

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as M
import Data.Vector (Vector)

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
connectSignals client var queue = do

  -- The InterfacesAdded signal
  DBus.addMatch client matchAdded $ \sig -> do
    let [path, props] = DBus.signalBody sig
    handleAdded var queue (fromVariant' path) (fromVariant' props)

  -- The InterfacesRemoved signal
  DBus.addMatch client matchRemoved $ \sig -> do
    let [path, props] = DBus.signalBody sig
    handleDeleted var queue (fromVariant' path) (fromVariant' props)

  -- And the PropertiesChanged signal
  DBus.addMatch client matchChanged $ \sig -> do
    -- The third parameter is called invalidated_properties. I don't know what
    -- to do with it, let's hope we don't need it.
    let [iface, props, _] = DBus.signalBody sig
        path = DBus.signalPath sig
    handleChanged var queue path (fromVariant' iface) (fromVariant' props)

  where matchAdded = DBus.matchAny
            { DBus.matchPath = Just "/org/freedesktop/UDisks2"
            , DBus.matchInterface = Just "org.freedesktop.DBus.ObjectManager"
            , DBus.matchMember = Just "InterfacesAdded"
            }
        matchRemoved = DBus.matchAny
            { DBus.matchPath = Just "/org/freedesktop/UDisks2"
            , DBus.matchInterface = Just "org.freedesktop.DBus.ObjectManager"
            , DBus.matchMember = Just "InterfacesRemoved"
            }
        matchChanged = DBus.matchAny
            { DBus.matchPathNamespace = Just "/org/freedesktop/UDisks2"
            , DBus.matchInterface = Just "org.freedesktop.DBus.Properties"
            , DBus.matchMember = Just "PropertiesChanged"
            }

getInitialObjects :: Client -> ExceptT Text IO ObjectMap
getInitialObjects client =
  lift (invoke client ObjectManager "GetManagedObjects" []) >>= \case
    Left e -> throwE (Text.pack $ show e)
    Right m -> ExceptT $ return $ runExcept $ parseObjectMap $
                 M.mapKeys ObjectId $ fromVariant' m


handleAdded :: TMVar ObjectMap -> TQueue Event -> DBus.ObjectPath -> InterfaceMap -> IO ()
handleAdded objMapVar events path ifaces = atomically $ do
  objMap <- takeTMVar objMapVar

  let objId = ObjectId path

  putTMVar objMapVar =<< case M.lookup objId objMap of

    -- Object not yet present, must be new
    Nothing -> case runExcept $ parseObject objId ifaces of
      Left _ -> return objMap -- TODO Handle error (maybe log it)
      Right newObj -> do
        writeTQueue events $ ObjectAdded path newObj
        return $ M.insert objId newObj objMap

    -- Object present, modify it
    Just oldObj -> case runExcept $ addInterfaces oldObj ifaces of
      Left e -> traceShow e $ return objMap -- TODO Handle error (maybe log it)
      Right newObj -> do
        writeTQueue events $ ObjectChanged path newObj
        return $ M.insert objId newObj objMap


handleDeleted :: TMVar ObjectMap -> TQueue Event -> DBus.ObjectPath
              -> Vector String -> IO ()
handleDeleted objMapVar events path ifaces = atomically $ do
  objMap <- takeTMVar objMapVar

  let objId = ObjectId path

  putTMVar objMapVar =<< case M.lookup objId objMap of

    -- Interfaces on unknown object removed. Igore.
    Nothing -> return objMap

    -- Ok, we know the object \o/
    Just oldObj -> case removeInterfaces oldObj ifaces of
      -- Object was completely removed
      Nothing -> do
        writeTQueue events $ ObjectRemoved path
        return $ M.delete objId objMap

      Just newObj -> do
        writeTQueue events $ ObjectChanged path newObj
        return $ M.insert objId newObj objMap

handleChanged :: TMVar ObjectMap -> TQueue Event -> DBus.ObjectPath
              -> String -> PropertyMap -> IO ()
handleChanged objMapVar events path iface props = atomically $ do
  objMap <- takeTMVar objMapVar

  let objId = ObjectId path

  putTMVar objMapVar =<< case M.lookup objId objMap of

    -- We don't know this object. Something is wrong
    Nothing -> return objMap -- TODO Handle error (maybe log it)

    Just oldObj -> case runExcept $ changeProperties oldObj iface props of
      Left _ -> return objMap -- TODO Handle error (maybe log it)
      Right newObj -> do
        writeTQueue events $ ObjectChanged path newObj
        return $ M.insert objId newObj objMap

data ObjectManager = ObjectManager

instance DBusObject ObjectManager where
  type DefaultInterface ObjectManager = ObjectManager
  getObjectPath _ = "/org/freedesktop/UDisks2"
  getDestination _ = "org.freedesktop.UDisks2"

instance DBusInterface ObjectManager where
  getInterface _ = "org.freedesktop.DBus.ObjectManager"

instance Implements ObjectManager ObjectManager
