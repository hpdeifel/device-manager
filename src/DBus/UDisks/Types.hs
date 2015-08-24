{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables, RankNTypes #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module DBus.UDisks.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import Data.List (isPrefixOf)

import Control.Monad.Trans.Except
import Control.Monad.State

import qualified DBus
import Control.Lens.TH
import Control.Lens (assign, Lens', ASetter, use)

-- TODO This module contains a lot of duplication.
--
-- For example, we always have two functions fill* and change* that do almost
-- the same. The first one initially creates an object, the second one changes
-- some properties on it.
--
-- The reason for this is that we get warnings if we forgot to initialize
-- something, but not if we only failed to change something. So if we reduce the
-- first case to the second, we would lose warnings.

-- TODO Newtype wrapper around DBus.ObjectPath. Something like
--      ObjectId or something

-- Used for the IdType property of the Block interface
data IdType = IdFilesystem | IdCrypto | IdRaid | IdOther Text
            deriving (Show)

-- TODO Better type than DBus.Variant for the configuration value
newtype Configuration = Configuration (Vector (Text, (Map Text DBus.Variant)))
                      deriving (Show)

data BlockIface = BlockIface {
  _blockDevice :: Text,
  _blockPreferredDevice :: Text,
  _blockSymlinks :: Vector Text,
  _blockDeviceNumber :: Word64,
  _blockDeviceId :: Text,
  _blockSize :: Word64,
  _blockReadOnly :: Bool,
  _blockDrive :: Maybe DBus.ObjectPath, -- "/" means Nothing
  _blockMdRaid :: Maybe DBus.ObjectPath, -- "/" means Nothing
  _blockMdRaidMember :: Maybe DBus.ObjectPath, -- "/" means Nothing
  _blockIdUsage :: Text,
  _blockIdType :: IdType,
  _blockIdVersion :: Text,
  _blockIdLabel :: Text,
  _blockIdUUID :: Text,
  _blockConfiguration :: Configuration,
  _blockCryptoBackingDevice :: Maybe DBus.ObjectPath,
  _blockHintPartititionable :: Bool,
  _blockHintSystem :: Bool,
  _blockHintIgnore :: Bool,
  _blockHintAuto :: Bool,
  _blockHintName :: Text,
  _blockHintIconName :: Text,
  _blockHintSymbolicIconName :: Text
} deriving (Show)

makeLenses ''BlockIface

data FileSystemIface = FileSystemIface {
  _fsMountPoints :: Vector Text
} deriving (Show)

makeLenses ''FileSystemIface

data PartititionIface = PartititionIface {
  _partititionNumber :: Word32,
  _partititionPartititionType :: Text,
  _partititionFlags :: Word64,
  _partititionOffset :: Word64,
  _partititionSize :: Word64,
  _partititionName :: Text,
  _partititionUUID :: Text,
  _partititionTable :: DBus.ObjectPath,
  _partititionIsContainer :: Bool,
  _partititionIsContained :: Bool
} deriving (Show)

makeLenses ''PartititionIface

data LoopIface = LoopIface {
  _loopBackingFile :: Text,
  _loopAutoclear :: Bool,
  _loopSetupByUID :: Word32
} deriving (Show)

makeLenses ''LoopIface

data BlockDevice = BlockDevice {
  _blockDevBlock :: BlockIface,
  _blockDevFS :: Maybe FileSystemIface,
  _blockDevPartitition :: Maybe PartititionIface,
  _blockDevLoop :: Maybe LoopIface
} deriving (Show)

makeLenses ''BlockDevice

data Object = BlockDevObject BlockDevice
            | DriveObject
            | MDRaidObject
            | JobObject
            | OtherObject DBus.ObjectPath
            deriving (Show)

parseIdType :: Text -> IdType
parseIdType t = case t of
  "filesystem" -> IdFilesystem
  "crypto"     -> IdCrypto
  "raid"       -> IdRaid
  _            -> IdOther t

parseConfiguration :: Vector (Text, (Map Text DBus.Variant)) -> Configuration
parseConfiguration = Configuration

changeBlockIface :: BlockIface -> PropertyMap -> FillM BlockIface
changeBlockIface iface props = flip execStateT iface $ do
  blockDevice <~? fmap decodeUtf8 <$> property' "Device"
  blockPreferredDevice <~? fmap decodeUtf8 <$> property' "PreferredDevice"
  blockSymlinks <~? fmap (fmap decodeUtf8) <$> property' "Symlinks"
  blockDeviceNumber <~? property' "DeviceNumber"
  blockDeviceId <~? property' "Id"
  blockSize <~? property' "Size"
  blockReadOnly <~? property' "ReadOnly"
  blockDrive <~? fmap maybeRoot <$> property' "Drive"
  blockMdRaid <~? fmap maybeRoot <$> property' "MDRaid"
  blockMdRaidMember <~? fmap maybeRoot <$> property' "MDRaidMember"
  blockIdUsage <~? property' "IdUsage"
  blockIdType <~? fmap parseIdType <$> property' "IdType"
  blockIdVersion <~? property' "IdVersion"
  blockIdLabel <~? property' "IdLabel"
  blockIdUUID <~? property' "IdUUID"
  blockConfiguration <~? fmap parseConfiguration <$> property' "Configuration"
  blockCryptoBackingDevice <~? fmap maybeRoot <$> property' "CryptoBackingDevice"
  blockHintPartititionable <~? property' "HintPartitionable"
  blockHintSystem <~? property' "HintSystem"
  blockHintIgnore <~? property' "HintIgnore"
  blockHintAuto <~? property' "HintAuto"
  blockHintName <~? property' "HintName"
  blockHintIconName <~? property' "HintIconName"
  blockHintSymbolicIconName <~? property' "HintSymbolicIconName"

  where property' :: DBus.IsVariant a => String -> StateT BlockIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillBlockIface :: PropertyMap -> FillM BlockIface
fillBlockIface props = do
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
  _blockDrive <- maybeRoot <$> property' "Drive"
  _blockMdRaid <- maybeRoot <$> property' "MDRaid"
  _blockMdRaidMember <- maybeRoot <$> property' "MDRaidMember"
  _blockIdUsage <- property' "IdUsage"
  _blockIdType <- parseIdType <$> property' "IdType"
  _blockIdVersion <- property' "IdVersion"
  _blockIdLabel <- property' "IdLabel"
  _blockIdUUID <- property' "IdUUID"
  _blockConfiguration <- parseConfiguration <$> property' "Configuration"
  _blockCryptoBackingDevice <- maybeRoot <$> property' "CryptoBackingDevice"
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
changeFSIface iface props = flip execStateT iface $ do
  fsMountPoints <~? fmap (V.map decodeUtf8) <$> property' "MountPoints"

  where property' :: DBus.IsVariant a => String -> StateT FileSystemIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillFSIface :: PropertyMap -> FillM FileSystemIface
fillFSIface props = do
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
  partititionTable <~? property' "Table"
  partititionIsContainer <~? property' "IsContainer"
  partititionIsContained <~? property' "IsContained"

  where property' :: DBus.IsVariant a => String -> StateT PartititionIface FillM (Maybe a)
        property' prop = lift $ maybeProperty props prop

fillPartititionIface :: PropertyMap -> FillM PartititionIface
fillPartititionIface props = do
  _partititionNumber <- property' "Number"
  _partititionPartititionType <- property' "PartititionType"
  _partititionFlags <- property' "Flags"
  _partititionOffset <- property' "Offset"
  _partititionSize <- property' "Size"
  _partititionName <- property' "Name"
  _partititionUUID <- property' "UUID"
  _partititionTable <- property' "Table"
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

fillLoopIface :: PropertyMap -> FillM LoopIface
fillLoopIface props = do
  _loopBackingFile <- decodeUtf8 <$> property' "BackingFile"
  _loopAutoclear <- property' "Autoclear"
  _loopSetupByUID <- property' "SetupByUID"
  return LoopIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

fillBlockDevice :: InterfaceMap -> FillM BlockDevice
fillBlockDevice ifaces = do
  _blockDevBlock <- required
  _blockDevFS <- interface'
  _blockDevPartitition <- interface'
  _blockDevLoop <- interface'
  return $ BlockDevice{..}

  where interface' :: (Show i, FillIface i) => FillM (Maybe i)
        interface' = interface ifaces

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
        interface' = lift $ interface ifaces

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
        maybeDelete i = if V.elem (ifaceName (Proxy :: Proxy i)) ifaces
                         then assign i Nothing
                         else return ()

-- FIXME This is bullshit. PropertiesChanged doesn't send all properties, but only
--       those that changed. So we need functions to change only _some_ properties in
--       interfaces.
changeBlockDevice :: BlockDevice -> String -> PropertyMap -> FillM BlockDevice
changeBlockDevice dev iface props = flip execStateT dev $ do
  changeInterface blockDevBlock
  changeInterfaceM blockDevFS
  changeInterfaceM blockDevPartitition
  changeInterfaceM blockDevLoop

  where changeInterface :: forall i. FillIface i => Lens' BlockDevice i -> StateT BlockDevice FillM ()
        changeInterface i
          | ifaceName (Proxy :: Proxy i) == iface = do
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

parseObject :: DBus.ObjectPath -> InterfaceMap -> FillM Object
parseObject path ifaces
  | prefix "block_devices" = BlockDevObject <$> fillBlockDevice ifaces
  | prefix "drives"        = return DriveObject
  | prefix "mdraid"        = return MDRaidObject
  | prefix "jobs"          = return JobObject
  | otherwise              = return $ OtherObject path

  where path' = DBus.formatObjectPath path
        prefix pre = ("/org/freedesktop/UDisks2/" ++ pre ++ "/") `isPrefixOf` path'

addInterfaces :: Object -> InterfaceMap -> FillM Object
addInterfaces (BlockDevObject dev) ifaces = BlockDevObject <$>
  addToBlockDevice dev ifaces
addInterfaces obj  _ = return obj

-- Returns Nothing if the last or a required interface was removed from an object
removeInterfaces :: Object -> Vector String -> Maybe Object
removeInterfaces (BlockDevObject dev) ifaces = BlockDevObject <$>
  removeFromBlockDevice dev ifaces
removeInterfaces _ _ = Nothing -- TODO

changeProperties :: Object -> String -> PropertyMap -> FillM Object
changeProperties (BlockDevObject dev) iface props = BlockDevObject <$>
  changeBlockDevice dev iface props
changeProperties obj _ _ = return obj

parseObjectMap :: ObjectIfaceMap -> FillM ObjectMap
parseObjectMap = M.traverseWithKey parseObject

type ObjectIfaceMap = Map DBus.ObjectPath InterfaceMap

type ObjectMap = Map DBus.ObjectPath Object

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
  fillIface :: PropertyMap -> FillM i
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

interface :: forall i. FillIface i => InterfaceMap -> FillM (Maybe i)
interface ifaces = maybe (return Nothing) (fmap Just . fillIface) $ M.lookup key ifaces
  where key = ifaceName (Proxy :: Proxy i)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8 . B.filter (not.nullbyte)
  where nullbyte = (==0)

maybeRoot :: DBus.ObjectPath -> Maybe DBus.ObjectPath
maybeRoot p
  | p == "/"  = Nothing
  | otherwise = Just p

infix 3 <~?
(<~?) :: MonadState s m => ASetter s s a b -> m (Maybe b) -> m ()
a <~? b = b >>= \case
  Just x  -> assign a x
  Nothing -> return ()
