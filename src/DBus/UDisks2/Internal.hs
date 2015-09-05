{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables, RankNTypes #-}

module DBus.UDisks2.Internal where

import DBus.UDisks2.Types as T
import DBus.DBusAbstraction

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Exception

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as M
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import Data.List (isPrefixOf)
import Data.String
import Data.Int
import Data.Maybe


import qualified DBus.Client as DBus
import qualified DBus
import DBus.Client (Client)

import Control.Concurrent.STM

import Control.Lens.TH
import Control.Lens (assign, Lens', ASetter, use, (^.))

-- Types

data Event = ObjectAdded ObjectId Object
           | ObjectRemoved ObjectId
           | ObjectChanged ObjectId Object
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
  atomically $ takeTMVar (conObjectMap con)
  DBus.removeMatch (conClient con) (conSigHandler con)
  DBus.disconnect $ conClient con

withConnection :: ((Connection, ObjectMap) -> IO a)
               -> IO (Either Text a)
withConnection body = bracket connect (traverse $ disconnect .fst)
  (traverse body)

nextEvent :: Connection -> STM Event
nextEvent = readTQueue . conEventQueue

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
        writeTQueue events $ ObjectAdded objId newObj
        return $ M.insert objId newObj objMap

    -- Object present, modify it
    Just oldObj -> case runExcept $ addInterfaces oldObj ifaces of
      Left e -> return objMap -- TODO Handle error (maybe log it)
      Right newObj -> do
        writeTQueue events $ ObjectChanged objId newObj
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
        writeTQueue events $ ObjectRemoved objId
        return $ M.delete objId objMap

      Just newObj -> do
        writeTQueue events $ ObjectChanged objId newObj
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
        writeTQueue events $ ObjectChanged objId newObj
        return $ M.insert objId newObj objMap

data ObjectManager = ObjectManager

instance DBusObject ObjectManager where
  type DefaultInterface ObjectManager = ObjectManager
  getObjectPath _ = "/org/freedesktop/UDisks2"
  getDestination _ = "org.freedesktop.UDisks2"

instance DBusInterface ObjectManager where
  getInterface _ = "org.freedesktop.DBus.ObjectManager"

instance Implements ObjectManager ObjectManager




-- Parsing the types


parseIdType :: Text -> IdType
parseIdType t = case t of
  "filesystem" -> IdFilesystem
  "crypto"     -> IdCrypto
  "raid"       -> IdRaid
  _            -> IdOther t

parseConfiguration :: Vector (Text, Map Text DBus.Variant) -> Configuration
parseConfiguration = Configuration

parseMedia :: Text -> Media
parseMedia t = fromMaybe (Other t) $ lookup t mediaMap
  where mediaMap =
           [ (""                       , NoMedia)
           , ("thumb"                  , Thumb)
           , ("flash"                  , Flash)
           , ("flash_cf"               , FlashCF)
           , ("flash_ms"               , FlashMS)
           , ("flash_sm"               , FlashSM)
           , ("flash_sd"               , FlashSD)
           , ("flash_sdhc"             , FlashSdhc)
           , ("flash_sdxc"             , FlashSdxc)
           , ("flash_mmc"              , FlashMmc)
           , ("floppy"                 , Floppy)
           , ("floppy_zip"             , FloppyZip)
           , ("floppy_jaz"             , FloppyJaz)
           , ("optical"                , Optical)
           , ("optical_cd"             , OpticalCd)
           , ("optical_cd_r"           , OpticalCdR)
           , ("optical_cd_rw"          , OpticalCdRw)
           , ("optical_dvd"            , OpticalDvd)
           , ("optical_dvd_r"          , OpticalDvdR)
           , ("optical_dvd_rw"         , OpticalDvdRw)
           , ("optical_dvd_ram"        , OpticalDvdRam)
           , ("optical_dvd_plus_r"     , OpticalDvdPlusR)
           , ("optical_dvd_plus_rw"    , OpticalDvdPlusRw)
           , ("optical_dvd_plus_r_dl"  , OpticalDvdPlusRDl)
           , ("optical_dvd_plus_rw_dl" , OpticalDvdPlusRwDl)
           , ("optical_bd"             , OpticalBd)
           , ("optical_bd_r"           , OpticalBdR)
           , ("optical_bd_re"          , OpticalBdRe)
           , ("optical_hddvd"          , OpticalHddvd)
           , ("optical_hddvd_r"        , OpticalHddvdR)
           , ("optical_hddvd_rw"       , OpticalHddvdRw)
           , ("optical_mo"             , OpticalMo)
           , ("optical_mrw"            , OpticalMrw)
           , ("optical_mrw_w"          , OpticalMrwW)
           ]


changeBlockIface :: BlockIface -> PropertyMap -> FillM BlockIface
changeBlockIface iface props = flip execStateT iface $ do
  blockDevice <~? fmap decodeUtf8 <$> property' "Device"
  blockPreferredDevice <~? fmap decodeUtf8 <$> property' "PreferredDevice"
  blockSymlinks <~? fmap (fmap decodeUtf8) <$> property' "Symlinks"
  blockDeviceNumber <~? property' "DeviceNumber"
  blockDeviceId <~? property' "Id"
  blockSize <~? property' "Size"
  blockReadOnly <~? property' "ReadOnly"
  blockDrive <~? fmap (maybeRoot.ObjectId) <$> property' "Drive"
  blockMdRaid <~? fmap (maybeRoot.ObjectId) <$> property' "MDRaid"
  blockMdRaidMember <~? fmap (maybeRoot.ObjectId) <$> property' "MDRaidMember"
  blockIdUsage <~? property' "IdUsage"
  blockIdType <~? fmap parseIdType <$> property' "IdType"
  blockIdVersion <~? property' "IdVersion"
  blockIdLabel <~? property' "IdLabel"
  blockIdUUID <~? property' "IdUUID"
  blockConfiguration <~? fmap parseConfiguration <$> property' "Configuration"
  blockCryptoBackingDevice <~? fmap (maybeRoot.ObjectId) <$> property' "CryptoBackingDevice"
  blockHintPartititionable <~? property' "HintPartitionable"
  blockHintSystem <~? property' "HintSystem"
  blockHintIgnore <~? property' "HintIgnore"
  blockHintAuto <~? property' "HintAuto"
  blockHintName <~? property' "HintName"
  blockHintIconName <~? property' "HintIconName"
  blockHintSymbolicIconName <~? property' "HintSymbolicIconName"

  where property' :: DBus.IsVariant a => String -> StateT BlockIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillBlockIface :: ObjectId -> PropertyMap -> FillM BlockIface
fillBlockIface _blockObj props = do
  -- NOTE We assume here, that the filesystem paths are encoded in utf8
  -- This may of course be violated on some systems, but those may not
  -- be worthwhile to support for a udisks helper program
  _blockDevice <- decodeUtf8 <$> property' "Device"
  _blockPreferredDevice <- decodeUtf8 <$> property' "PreferredDevice"
  _blockSymlinks <- fmap decodeUtf8 <$> property' "Symlinks"
  _blockDeviceNumber <- property' "DeviceNumber"
  _blockDeviceId <- property' "Id"
  _blockSize <- property' "Size"
  _blockReadOnly <- property' "ReadOnly"
  _blockDrive <- maybeRoot.ObjectId <$> property' "Drive"
  _blockMdRaid <- maybeRoot.ObjectId <$> property' "MDRaid"
  _blockMdRaidMember <- maybeRoot.ObjectId <$> property' "MDRaidMember"
  _blockIdUsage <- property' "IdUsage"
  _blockIdType <- parseIdType <$> property' "IdType"
  _blockIdVersion <- property' "IdVersion"
  _blockIdLabel <- property' "IdLabel"
  _blockIdUUID <- property' "IdUUID"
  _blockConfiguration <- parseConfiguration <$> property' "Configuration"
  _blockCryptoBackingDevice <- maybeRoot.ObjectId <$> property' "CryptoBackingDevice"
  _blockHintPartititionable <- property' "HintPartitionable"
  _blockHintSystem <- property' "HintSystem"
  _blockHintIgnore <- property' "HintIgnore"
  _blockHintAuto <- property' "HintAuto"
  _blockHintName <- property' "HintName"
  _blockHintIconName <- property' "HintIconName"
  _blockHintSymbolicIconName <- property' "HintSymbolicIconName"
  return BlockIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

changeFSIface :: FileSystemIface -> PropertyMap -> FillM FileSystemIface
changeFSIface iface props = flip execStateT iface $
  fsMountPoints <~? fmap (V.map decodeUtf8) <$> property' "MountPoints"

  where property' :: DBus.IsVariant a => String -> StateT FileSystemIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillFSIface :: ObjectId -> PropertyMap -> FillM FileSystemIface
fillFSIface _fsObj props = do
  _fsMountPoints <- V.map decodeUtf8 <$> property' "MountPoints"
  return FileSystemIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

changePartititionIface :: PartititionIface -> PropertyMap -> FillM PartititionIface
changePartititionIface iface props = flip execStateT iface $ do
  partititionNumber <~? property' "Number"
  partititionPartititionType <~? property' "PartititionType"
  partititionFlags <~? property' "Flags"
  partititionOffset <~? property' "Offset"
  partititionSize <~? property' "Size"
  partititionName <~? property' "Name"
  partititionUUID <~? property' "UUID"
  partititionTable <~? fmap ObjectId <$> property' "Table"
  partititionIsContainer <~? property' "IsContainer"
  partititionIsContained <~? property' "IsContained"

  where property' :: DBus.IsVariant a => String -> StateT PartititionIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillPartititionIface :: ObjectId -> PropertyMap -> FillM PartititionIface
fillPartititionIface _partititionObj props = do
  _partititionNumber <- property' "Number"
  _partititionPartititionType <- property' "PartititionType"
  _partititionFlags <- property' "Flags"
  _partititionOffset <- property' "Offset"
  _partititionSize <- property' "Size"
  _partititionName <- property' "Name"
  _partititionUUID <- property' "UUID"
  _partititionTable <- ObjectId <$> property' "Table"
  _partititionIsContainer <- property' "IsContainer"
  _partititionIsContained <- property' "IsContained"
  return PartititionIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

changeLoopIface :: LoopIface -> PropertyMap -> FillM LoopIface
changeLoopIface iface props = flip execStateT iface $ do
  loopBackingFile <~? fmap decodeUtf8 <$> property' "BackingFile"
  loopAutoclear <~? property' "Autoclear"
  loopSetupByUID <~? property' "SetupByUID"

  where property' :: DBus.IsVariant a => String -> StateT LoopIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillLoopIface :: ObjectId -> PropertyMap -> FillM LoopIface
fillLoopIface _loopObj props = do
  _loopBackingFile <- decodeUtf8 <$> property' "BackingFile"
  _loopAutoclear <- property' "Autoclear"
  _loopSetupByUID <- property' "SetupByUID"
  return LoopIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

fillBlockDevice :: ObjectId -> InterfaceMap -> FillM BlockDevice
fillBlockDevice _blockDevObj ifaces = do
  _blockDevBlock <- required
  _blockDevFS <- interface'
  _blockDevPartitition <- interface'
  _blockDevLoop <- interface'
  return BlockDevice{..}

  where interface' :: (Show i, FillIface i) => FillM (Maybe i)
        interface' = interface _blockDevObj ifaces

        required :: forall i. (Show i, FillIface i) => FillM i
        required = interface' >>= \case
          Nothing -> throwE $ "Interface " <> T.pack iname <> " is required but doesn't exist"
          Just i' -> return i'
          where iname = ifaceName (Proxy :: Proxy i)

addToBlockDevice :: BlockDevice -> InterfaceMap -> FillM BlockDevice
addToBlockDevice dev ifaces = flip execStateT dev $ do
  blockDevBlock <~? interface'
  blockDevFS <~? fmap Just interface'
  blockDevPartitition <~? fmap Just interface'
  blockDevLoop <~? fmap Just interface'

  where interface' :: (Show i, FillIface i) => StateT BlockDevice FillM (Maybe i)
        interface' = lift $ interface (dev^.blockDevObj) ifaces

removeFromBlockDevice :: BlockDevice -> Vector String -> Maybe BlockDevice
removeFromBlockDevice dev ifaces
  -- The 'Block' Interface is required. If it got removed, return Nothing
  | blockIface `V.elem` ifaces  = Nothing
  | otherwise                   = Just $ flip execState dev $ do

      maybeDelete blockDevFS
      maybeDelete blockDevPartitition
      maybeDelete blockDevLoop

  where blockIface = ifaceName (Proxy :: Proxy BlockIface)

        maybeDelete :: forall i. FillIface i => Lens' BlockDevice (Maybe i)
                    -> State BlockDevice ()
        maybeDelete i = when (V.elem (ifaceName (Proxy :: Proxy i)) ifaces) $
                         assign i Nothing

changeBlockDevice :: BlockDevice -> String -> PropertyMap -> FillM BlockDevice
changeBlockDevice dev iface props = flip execStateT dev $ do
  changeInterface blockDevBlock
  changeInterfaceM blockDevFS
  changeInterfaceM blockDevPartitition
  changeInterfaceM blockDevLoop

  where changeInterface :: forall i. FillIface i => Lens' BlockDevice i -> StateT BlockDevice FillM ()
        changeInterface i
          | ifaceName (Proxy :: Proxy i) == iface =
              use i >>= lift . flip changeIface props >>= assign i
          | otherwise = return ()

        changeInterfaceM :: forall i. FillIface i => Lens' BlockDevice (Maybe i) -> StateT BlockDevice FillM ()
        changeInterfaceM i
          | ifaceName (Proxy :: Proxy i) == iface = use i >>= \case
              Just i' -> lift (Just <$> changeIface i' props) >>= assign i
              -- Interface wasn't present. If we try to change it, we cannot try
              -- to change it, since we only know the changed properties and not
              -- the rest.
              Nothing -> return ()
          | otherwise = return ()

changeDriveIface :: DriveIface -> PropertyMap -> FillM DriveIface
changeDriveIface iface props = flip execStateT iface $ do
  driveIVendor <~? property' "Vendor"
  driveIModel <~? property' "Model"
  driveIRevision <~? property' "Revision"
  driveISerial <~? property' "Serial"
  driveIWwn <~? property' "WWN"
  driveIId <~? property' "Id"
  driveIConfiguration <~? property' "Configuration"
  driveIMedia <~? fmap parseMedia <$> property' "Media"
  driveIMediaCompatibility <~? fmap (V.map parseMedia) <$> property' "MediaCompatibility"
  driveIMediaRemovable <~? property' "MediaRemovable"
  driveIMediaAvailable <~? property' "MediaAvailable"
  driveIMediaChangeDetected <~? property' "MediaChangeDetected"
  driveISize <~? property' "Size"
  driveITimeDetected <~? property' "TimeDetected"
  driveITimeMediaDetected <~? property' "TimeMediaDetected"
  driveIOptical <~? property' "Optical"
  driveIOpticalBlank <~? property' "OpticalBlank"
  driveIOpticalNumTracks <~? property' "OpticalNumTracks"
  driveIOpticalNumAudioTracks <~? property' "OpticalNumAudioTracks"
  driveIOpticalNumDataTracks  <~? property' "OpticalNumDataTracks"
  driveIOpticalNumSessions <~? property' "OpticalNumSessions"
  driveIRotationRate <~? property' "RotationRate"
  driveIConnectionBus <~? property' "ConnectionBus"
  driveISeat <~? property' "Seat"
  driveIRemovable <~? property' "Removable"
  driveIEjectable <~? property' "Ejectable"
  driveISortKey <~? property' "SortKey"
  driveICanPowerOff <~? property' "CanPowerOff"
  driveISiblingId <~? property' "SiblingId"

  where property' :: DBus.IsVariant a => String -> StateT DriveIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillDriveIface :: ObjectId -> PropertyMap -> FillM DriveIface
fillDriveIface _driveIObj props = do
  _driveIVendor <- property' "Vendor"
  _driveIModel <- property' "Model"
  _driveIRevision <- property' "Revision"
  _driveISerial <- property' "Serial"
  _driveIWwn <- property' "WWN"
  _driveIId <- property' "Id"
  _driveIConfiguration <- property' "Configuration"
  _driveIMedia <- parseMedia <$> property' "Media"
  _driveIMediaCompatibility <- V.map parseMedia <$> property' "MediaCompatibility"
  _driveIMediaRemovable <- property' "MediaRemovable"
  _driveIMediaAvailable <- property' "MediaAvailable"
  _driveIMediaChangeDetected <- property' "MediaChangeDetected"
  _driveISize <- property' "Size"
  _driveITimeDetected <- property' "TimeDetected"
  _driveITimeMediaDetected <- property' "TimeMediaDetected"
  _driveIOptical <- property' "Optical"
  _driveIOpticalBlank <- property' "OpticalBlank"
  _driveIOpticalNumTracks <- property' "OpticalNumTracks"
  _driveIOpticalNumAudioTracks <- property' "OpticalNumAudioTracks"
  _driveIOpticalNumDataTracks  <- property' "OpticalNumDataTracks"
  _driveIOpticalNumSessions <- property' "OpticalNumSessions"
  _driveIRotationRate <- property' "RotationRate"
  _driveIConnectionBus <- property' "ConnectionBus"
  _driveISeat <- property' "Seat"
  _driveIRemovable <- property' "Removable"
  _driveIEjectable <- property' "Ejectable"
  _driveISortKey <- property' "SortKey"
  _driveICanPowerOff <- property' "CanPowerOff"
  _driveISiblingId <- property' "SiblingId"

  return DriveIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

changeAtaIface :: AtaIface -> PropertyMap -> FillM AtaIface
changeAtaIface iface props = flip execStateT iface $ do
  ataSmartSupported <~? property' "SmartSupported"
  ataSmartEnabled <~? property' "SmartEnabled"
  ataSmartUpdated <~? property' "SmartUpdated"
  ataSmartFailing <~? property' "SmartFailing"
  ataSmartPowerOnSeconds <~? property' "SmartPowerOnSeconds"
  ataSmartTemperature <~? property' "SmartTemperature"
  ataSmartNumAttributesFailing <~? property' "SmartNumAttributesFailing"
  ataSmartNumAttributesFailedInThePast <~? property' "SmartNumAttributesFailedInThePast"
  ataSmartNumBadSectors <~? property' "SmartNumBadSectors"
  ataSmartSelftestStatus <~? property' "SmartSelftestStatus"
  ataSmartSelftestPercentRemaining <~? property' "SmartSelftestPercentRemaining"
  ataPmSupported <~? property' "PmSupported"
  ataPmEnabled <~? property' "PmEnabled"
  ataApmSupported <~? property' "ApmSupported"
  ataApmEnabled <~? property' "ApmEnabled"
  ataAamSupported <~? property' "AamSupported"
  ataAamEnabled <~? property' "AamEnabled"
  ataAamVendorRecommendedValue <~? property' "AamVendorRecommendedValue"
  ataWriteCacheSupported <~? property' "WriteCacheSupported"
  ataWriteCacheEnabled <~? property' "WriteCacheEnabled"
  ataSecurityEraseUnitMinutes <~? property' "SecurityEraseUnitMinutes"
  ataSecurityEnhancedEraseUnitMinutes <~? property' "SecurityEnhancedEraseUnitMinutes"
  ataSecurityFrozen <~? property' "SecurityFrozen"

  where property' :: DBus.IsVariant a => String -> StateT AtaIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillAtaIface :: ObjectId -> PropertyMap -> FillM AtaIface
fillAtaIface _ataObj props = do
  _ataSmartSupported <- property' "SmartSupported"
  _ataSmartEnabled <- property' "SmartEnabled"
  _ataSmartUpdated <- property' "SmartUpdated"
  _ataSmartFailing <- property' "SmartFailing"
  _ataSmartPowerOnSeconds <- property' "SmartPowerOnSeconds"
  _ataSmartTemperature <- property' "SmartTemperature"
  _ataSmartNumAttributesFailing <- property' "SmartNumAttributesFailing"
  _ataSmartNumAttributesFailedInThePast <- property' "SmartNumAttributesFailedInThePast"
  _ataSmartNumBadSectors <- property' "SmartNumBadSectors"
  _ataSmartSelftestStatus <- property' "SmartSelftestStatus"
  _ataSmartSelftestPercentRemaining <- property' "SmartSelftestPercentRemaining"
  _ataPmSupported <- property' "PmSupported"
  _ataPmEnabled <- property' "PmEnabled"
  _ataApmSupported <- property' "ApmSupported"
  _ataApmEnabled <- property' "ApmEnabled"
  _ataAamSupported <- property' "AamSupported"
  _ataAamEnabled <- property' "AamEnabled"
  _ataAamVendorRecommendedValue <- property' "AamVendorRecommendedValue"
  _ataWriteCacheSupported <- property' "WriteCacheSupported"
  _ataWriteCacheEnabled <- property' "WriteCacheEnabled"
  _ataSecurityEraseUnitMinutes <- property' "SecurityEraseUnitMinutes"
  _ataSecurityEnhancedEraseUnitMinutes <- property' "SecurityEnhancedEraseUnitMinutes"
  _ataSecurityFrozen <- property' "SecurityFrozen"

  return AtaIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

fillDrive :: ObjectId -> InterfaceMap -> FillM Drive
fillDrive _driveObj ifaces = do
  _driveDrive <- required
  _driveAta <- interface'

  return Drive{..}

  where interface' :: (Show i, FillIface i) => FillM (Maybe i)
        interface' = interface _driveObj ifaces

        required :: forall i. (Show i, FillIface i) => FillM i
        required = interface' >>= \case
          Nothing -> throwE $ "Interface " <> T.pack iname <> " is required but doesn't exist"
          Just i' -> return i'
          where iname = ifaceName (Proxy :: Proxy i)

addToDrive :: Drive -> InterfaceMap -> FillM Drive
addToDrive drive ifaces = flip execStateT drive $ do
  driveDrive <~? interface'
  driveAta   <~? Just <$> interface'

  where interface' :: (Show i, FillIface i) => StateT Drive FillM (Maybe i)
        interface' = lift $ interface (drive^.driveObj) ifaces

removeFromDrive :: Drive -> Vector String -> Maybe Drive
removeFromDrive drive ifaces
  | driveIface `V.elem` ifaces = Nothing
  | otherwise                  = Just $ flip execState drive $
      maybeDelete driveAta

  where driveIface = ifaceName (Proxy :: Proxy DriveIface)

        maybeDelete :: forall i. FillIface i => Lens' Drive (Maybe i)
                    -> State Drive ()
        maybeDelete i = when (V.elem (ifaceName (Proxy :: Proxy i)) ifaces) $
                         assign i Nothing

changeDrive :: Drive -> String -> PropertyMap -> FillM Drive
changeDrive drive iface props = flip execStateT drive $ do
  changeInterface driveDrive
  changeInterfaceM driveAta

  where changeInterface :: forall i. FillIface i => Lens' Drive i
                        -> StateT Drive FillM ()
        changeInterface i
          | ifaceName (Proxy :: Proxy i) == iface =
              use i >>= lift . flip changeIface props >>= assign i
          | otherwise = return ()

        changeInterfaceM :: forall i. FillIface i => Lens' Drive (Maybe i)
                         -> StateT Drive FillM ()
        changeInterfaceM i
          | ifaceName (Proxy :: Proxy i) == iface = use i >>= \case
              Just i' -> lift (Just <$> changeIface i' props) >>= assign i
              -- Interface wasn't present. If we try to change it, we cannot try
              -- to change it, since we only know the changed properties and not
              -- the rest.
              Nothing -> return ()
          | otherwise = return ()

parseObject :: ObjectId -> InterfaceMap -> FillM Object
parseObject objId@(ObjectId path) ifaces
  | prefix "block_devices" = BlockDevObject <$> fillBlockDevice objId ifaces
  | prefix "drives"        = DriveObject <$> fillDrive objId ifaces
  | prefix "mdraid"        = return MDRaidObject
  | prefix "jobs"          = return JobObject
  | otherwise              = return $ OtherObject $ ObjectId path

  where path' = DBus.formatObjectPath path
        prefix pre = ("/org/freedesktop/UDisks2/" ++ pre ++ "/") `isPrefixOf` path'

addInterfaces :: Object -> InterfaceMap -> FillM Object
addInterfaces (BlockDevObject dev) ifaces = BlockDevObject <$>
  addToBlockDevice dev ifaces
addInterfaces (DriveObject drive) ifaces = DriveObject <$>
  addToDrive drive ifaces
addInterfaces obj  _ = return obj

-- Returns Nothing if the last or a required interface was removed from an object
removeInterfaces :: Object -> Vector String -> Maybe Object
removeInterfaces (BlockDevObject dev) ifaces = BlockDevObject <$>
  removeFromBlockDevice dev ifaces
removeInterfaces (DriveObject drive) ifaces = DriveObject <$>
  removeFromDrive drive ifaces
removeInterfaces _ _ = Nothing -- TODO

changeProperties :: Object -> String -> PropertyMap -> FillM Object
changeProperties (BlockDevObject dev) iface props = BlockDevObject <$>
  changeBlockDevice dev iface props
changeProperties (DriveObject drive) iface props = DriveObject <$>
  changeDrive drive iface props
changeProperties obj _ _ = return obj

parseObjectMap :: ObjectIfaceMap -> FillM ObjectMap
parseObjectMap = M.traverseWithKey parseObject

type ObjectIfaceMap = Map ObjectId InterfaceMap

type ObjectMap = Map ObjectId Object

type PropertyMap = Map String DBus.Variant

-- | A map from interface names to a property map
type InterfaceMap = Map String PropertyMap

type FillM = Except Text

property :: DBus.IsVariant a => PropertyMap -> String -> FillM a
property m k = maybeProperty m k >>= \case
  Just x -> return x
  Nothing -> throwE $ notfound k

  where notfound :: String -> Text
        notfound p = "Expected property " <> T.pack p <> " but it wasn't there"


maybeProperty :: DBus.IsVariant a => PropertyMap -> String -> FillM (Maybe a)
maybeProperty m k = traverse maybeVariant $ M.lookup k m

  where maybeVariant :: DBus.IsVariant a => DBus.Variant -> FillM a
        maybeVariant v = case DBus.fromVariant v of
          Nothing -> throwE $ conversion v
          Just v' -> return v'

        conversion v = "Failed type conversion for: " <> T.pack (show v) <> " (Property "
                       <> T.pack k <> ")"

class FillIface i where
  fillIface :: ObjectId -> PropertyMap -> FillM i
  changeIface :: i -> PropertyMap -> FillM i
  ifaceName :: Proxy i -> String

instance FillIface BlockIface where
  fillIface = fillBlockIface
  changeIface = changeBlockIface
  ifaceName _ = "org.freedesktop.UDisks2.Block"

instance FillIface FileSystemIface where
  fillIface = fillFSIface
  changeIface = changeFSIface
  ifaceName _ = "org.freedesktop.UDisks2.Filesystem"

instance FillIface PartititionIface where
  fillIface = fillPartititionIface
  changeIface = changePartititionIface
  ifaceName _ = "org.freedesktop.UDisks2.Partitition"

instance FillIface LoopIface where
  fillIface = fillLoopIface
  changeIface = changeLoopIface
  ifaceName _ =  "org.freedesktop.UDisks2.Loop"

instance FillIface DriveIface where
  fillIface = fillDriveIface
  changeIface = changeDriveIface
  ifaceName _ = "org.freedesktop.UDisks2.Drive"

instance FillIface AtaIface where
  fillIface = fillAtaIface
  changeIface = changeAtaIface
  ifaceName _ = "org.freedesktop.UDisks2.Drive.Ata"

interface :: forall i. FillIface i => ObjectId -> InterfaceMap -> FillM (Maybe i)
interface objId ifaces = maybe (return Nothing) (fmap Just . fillIface objId) $ M.lookup key ifaces
  where key = ifaceName (Proxy :: Proxy i)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8 . B.filter (not.nullbyte)
  where nullbyte = (==0)

maybeRoot :: ObjectId -> Maybe ObjectId
maybeRoot p
  | p == "/"  = Nothing
  | otherwise = Just p

infix 3 <~?
(<~?) :: MonadState s m => ASetter s s a b -> m (Maybe b) -> m ()
a <~? b = b >>= \case
  Just x  -> assign a x
  Nothing -> return ()
