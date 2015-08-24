{-# LANGUAGE LambdaCase #-}


-- | This module provides a much simpler interface to the udisks daemon.
module DBus.UDisks2.Simple
       ( Device(Device,devMountPoints,devFile,devName)
       , Event(..)
       , Connection
       , connect
       , disconnect
       , withConnection
       , nextEvent
       ) where

import qualified DBus.UDisks2 as U
import qualified DBus.UDisks2.Types as U

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Function
import Data.Traversable
import Control.Concurrent.STM
import Control.Exception

import Control.Lens

-- Represents only removeable devices
data Device = Device {
  devId :: U.ObjectId,
  devMountPoints :: Vector Text,
  devFile :: Text,
  devName :: Text
}

instance Eq Device where (==) = (==) `on` devId
instance Ord Device where compare = compare `on` devId

data Event = DeviceAdded Device
           | DeviceChanged Device Device -- ^ Old, New
           | DeviceRemoved Device

data Connection = Connection {
  conUDisks :: U.Connection,
  conDevices :: TMVar (Map U.ObjectId Device)
}

connect :: IO (Either Text (Connection, [Device]))
connect = U.connect >>= \case
  Left e -> return $ Left e
  Right (udisks, objMap) -> do
    let objects = M.mapMaybeWithKey convertDevice objMap
    var <- newTMVarIO objects

    let con = Connection
          { conUDisks = udisks
          , conDevices = var
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
            devs <- takeTMVar (conDevices con)
            let newDev = convertDevice objId obj
            putTMVar (conDevices con) $
              M.alter (const newDev) objId devs
            return $ DeviceAdded <$> newDev

          U.ObjectRemoved objId -> do
            devs <- takeTMVar (conDevices con)
            for (M.lookup objId devs)  $ \obj -> do
              putTMVar (conDevices con) $ M.delete objId devs
              return $ DeviceRemoved obj

          U.ObjectChanged objId obj -> do
            devs <- takeTMVar (conDevices con)
            let newDev = convertDevice objId obj
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

convertDevice :: U.ObjectId -> U.Object -> Maybe Device
convertDevice objId (U.BlockDevObject obj)
  | boring obj  = Nothing
  | otherwise   = Just $ Device
                  { devId = objId
                  , devMountPoints = maybe V.empty id $
                      obj ^. U.blockDevFS ^? _Just . U.fsMountPoints
                  , devFile = obj ^. U.blockDevBlock . U.blockPreferredDevice
                  , devName = obj ^. U.blockDevBlock . U.blockHintName
                  }
convertDevice _ _ = Nothing

boring :: U.BlockDevice -> Bool
boring dev = and
  [ dev ^. U.blockDevFS & isn't _Just
  , dev ^. U.blockDevBlock . U.blockHintSystem
  , dev ^. U.blockDevBlock . U.blockHintIgnore
  -- TODO "Removable" property of Drive
  ]
