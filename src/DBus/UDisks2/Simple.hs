{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes #-}


-- | This module provides a much simpler interface to the udisks daemon.
module DBus.UDisks2.Simple
       ( Device(Device,devMountPoints,devFile,devName,devSize,devMediaType)
       , devMounted
       , Event(..)
       , Connection
       , Media(..)
       , connect
       , disconnect
       , withConnection
       , nextEvent

       , mount
       , unmount
       ) where

import qualified DBus.UDisks2 as U
import qualified DBus.UDisks2.Types as U
import DBus.UDisks2.Types (Media)
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
import Data.Word
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
  devName :: Text,
  devSize :: Word64,
  devMediaType :: Media
} deriving (Show)

instance Eq Device where (==) = (==) `on` devId
instance Ord Device where compare = compare `on` devId

devMounted :: Device -> Bool
devMounted = not . V.null . devMountPoints

data Event = DeviceAdded Device
           | DeviceChanged Device Device -- ^ Old, New
           | DeviceRemoved Device
           deriving Show

type ObjectMap = Map U.ObjectId U.Object

data Connection = Connection {
  conUDisks :: U.Connection,
  conDevices :: TMVar (Map U.ObjectId Device),
  conObjMap :: TMVar ObjectMap
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
  void $ atomically $ takeTMVar (conDevices con)
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
            for (M.lookup objId devs)  $ \obj ->
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

              Just oldDev ->
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
  case objMap ^. at (devId dev) ^? _Just . U._BlockDevObject . U.blockDevFS ^. to join of
    Just fileSystem -> U.runOperation (conUDisks con) $
                       U.fsMount fileSystem M.empty
    Nothing -> return $ Left $ "Device " <> devName dev <> " doesn't support mounting"

unmount :: Connection -> Device -> IO (Either Text ())
unmount con dev = do
  objMap <- atomically $ readTMVar (conObjMap con)
  case objMap ^. at (devId dev) ^? _Just . U._BlockDevObject . U.blockDevFS ^. to join of
    Just fileSystem -> U.runOperation (conUDisks con) $
                       U.fsUnmount fileSystem M.empty
    Nothing -> return $ Left $ "Device " <> devName dev <> " doesn't support unmounting"

convertDevice :: ObjectMap -> U.ObjectId -> U.Object -> Maybe Device
convertDevice objMap objId (U.BlockDevObject obj)
  | boring objMap obj  = Nothing
  | otherwise          = Just Device
     { devId = objId
     , devMountPoints = fromMaybe V.empty $
          obj ^. U.blockDevFS ^? _Just . U.fsMountPoints
     , devFile = obj ^. U.blockDevBlock . U.blockPreferredDevice
     , devName = obj ^. U.blockDevBlock . U.blockIdLabel
     , devSize = obj ^. U.blockDevBlock . U.blockSize
     , devMediaType = obj ^. U.blockDevBlock . blockDrive' objMap
                          ^? _Just . U.driveDrive . U.driveIMedia
                          ^. to (fromMaybe U.NoMedia)
     }
convertDevice _ _ _ = Nothing

boring :: ObjectMap -> U.BlockDevice -> Bool
boring objMap dev = or
  [ dev ^. U.blockDevFS & isn't _Just
  , dev ^. U.blockDevBlock . U.blockHintSystem
  , dev ^. U.blockDevBlock . U.blockHintIgnore
  , not removable
  ]

  where removable = dev ^. U.blockDevBlock . blockDrive' objMap
                        ^? _Just . U.driveDrive . U.driveIRemovable
                        ^. to (fromMaybe False)

-- | Returns the new value
modifyTMVar :: TMVar a -> (a -> a) -> STM a
modifyTMVar var f = do
  new <- f <$> takeTMVar var
  putTMVar var new
  return new

blockDrive' :: ObjectMap -> Lens' U.BlockIface (Maybe U.Drive)
blockDrive' objMap = lens getter setter
  where getter :: U.BlockIface -> Maybe U.Drive
        getter block = block^.U.blockDrive
                            ^?_Just.to (`M.lookup` objMap)
                             ._Just.U._DriveObject

        setter :: U.BlockIface -> Maybe U.Drive -> U.BlockIface
        setter block drive = block & U.blockDrive .~ (drive^?_Just.U.driveObj)
