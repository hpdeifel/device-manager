{-# LANGUAGE LambdaCase, OverloadedStrings #-}


-- | This module provides a much simpler interface to the udisks daemon.
module DBus.UDisks2.Simple
       ( Device(Device,devMountPoints,devFile,devName)
       , devMounted
       , Event(..)
       , Connection
       , connect
       , disconnect
       , withConnection
       , nextEvent

       , mount
       ) where

import qualified DBus.UDisks2 as U
import qualified DBus.UDisks2.Types as U
import qualified DBus.UDisks2.Operations as U

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Function
import Data.Traversable
import Data.Monoid
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Lens

type MountPoint = Text

-- Represents only removeable devices
data Device = Device {
  devId :: U.ObjectId,
  devMountPoints :: Vector MountPoint,
  devFile :: Text,
  devName :: Text
} deriving (Show)

instance Eq Device where (==) = (==) `on` devId
instance Ord Device where compare = compare `on` devId

devMounted :: Device -> Bool
devMounted = V.null . devMountPoints

data Event = DeviceAdded Device
           | DeviceChanged Device Device -- ^ Old, New
           | DeviceRemoved Device
           deriving Show

data Connection = Connection {
  conUDisks :: U.Connection,
  conDevices :: TMVar (Map U.ObjectId Device),
  conObjMap :: TMVar U.ObjectMap
}

connect :: IO (Either Text (Connection, [Device]))
connect = U.connect >>= \case
  Left e -> return $ Left e
  Right (udisks, objMap) -> do
    let objects = M.mapMaybeWithKey (convertDevice objMap) objMap

    var <- newTMVarIO objects
    objMapVar <- newTMVarIO objMap

    let con = Connection
          { conUDisks = udisks
          , conDevices = var
          , conObjMap = objMapVar
          }

    return $ Right (con, M.elems objects)

disconnect :: Connection -> IO ()
disconnect con = do
  atomically $ takeTMVar (conDevices con)
  U.disconnect (conUDisks con)

withConnection :: ((Connection, [Device]) -> IO a)
               -> IO (Either Text a)
withConnection body = bracket connect (traverse $ disconnect . fst)
  (traverse body)

nextEvent :: Connection -> IO Event
nextEvent con = loop
  where innerLoop = U.nextEvent (conUDisks con) >>= \case
          U.ObjectAdded objId obj -> do
            objMap <- modifyTMVar (conObjMap con) $ M.insert objId obj
            devs <- takeTMVar (conDevices con)
            let newDev = convertDevice objMap objId obj
            putTMVar (conDevices con) $
              M.alter (const newDev) objId devs
            return $ DeviceAdded <$> newDev

          U.ObjectRemoved objId -> do
            void $ modifyTMVar (conObjMap con) $ M.delete objId
            devs <- takeTMVar (conDevices con)
            putTMVar (conDevices con) $ M.delete objId devs
            for (M.lookup objId devs)  $ \obj -> do
              return $ DeviceRemoved obj

          U.ObjectChanged objId obj -> do
            objMap <- modifyTMVar (conObjMap con) $ M.insert objId obj
            devs <- takeTMVar (conDevices con)
            let newDev = convertDevice objMap objId obj
            case M.lookup objId devs of
              Nothing -> do
                putTMVar (conDevices con) $
                  M.alter (const newDev) objId devs
                return $ DeviceAdded <$> newDev

              Just oldDev -> do
                for newDev $ \newDev' -> do
                  putTMVar (conDevices con) $
                    M.insert objId newDev' devs
                  return $ DeviceChanged oldDev newDev'

        loop = atomically innerLoop >>= \case
          Nothing -> loop
          Just res -> return res

-- Operations
mount :: Connection -> Device -> IO (Either Text MountPoint)
mount con dev = do
  objMap <- atomically $ readTMVar (conObjMap con)
  case (objMap ^. at (devId dev) ^? _Just . U._BlockDevObject . U.blockDevFS ^. to join) of
    Just fileSystem -> U.runOperation (conUDisks con) $
                       U.fsMount fileSystem M.empty
    Nothing -> return $ Left $ "Device " <> devName dev <> " doesn't support mounting"

convertDevice :: U.ObjectMap -> U.ObjectId -> U.Object -> Maybe Device
convertDevice objMap objId (U.BlockDevObject obj)
  | boring objMap obj  = Nothing
  | otherwise          = Just $ Device
     { devId = objId
     , devMountPoints = maybe V.empty id $
          obj ^. U.blockDevFS ^? _Just . U.fsMountPoints
     , devFile = obj ^. U.blockDevBlock . U.blockPreferredDevice
     , devName = obj ^. U.blockDevBlock . U.blockIdLabel
     }
convertDevice _ _ _ = Nothing

boring :: U.ObjectMap -> U.BlockDevice -> Bool
boring objMap dev = or
  [ dev ^. U.blockDevFS & isn't _Just
  , dev ^. U.blockDevBlock . U.blockHintSystem
  , dev ^. U.blockDevBlock . U.blockHintIgnore
  , not removable
  ]

  where removable = case dev ^. U.blockDevBlock . U.blockDrive of
          Just drive -> objMap ^. at drive
                               ^? _Just
                               . U._DriveObject
                               . U.driveDrive
                               . U.driveIRemovable
                               ^. to (fromMaybe False)
          Nothing -> False

-- | Returns the new value
modifyTMVar :: TMVar a -> (a -> a) -> STM a
modifyTMVar var f = do
  new <- f <$> takeTMVar var
  putTMVar var new
  return new
