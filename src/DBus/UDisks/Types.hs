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
import Control.Lens (assign, Lens')

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

fillFSIface :: PropertyMap -> FillM FileSystemIface
fillFSIface props = do
  _fsMountPoints <- V.map decodeUtf8 <$> property' "MountPoints"
  return FileSystemIface{..}

  where property' :: DBus.IsVariant a => String -> Except Text a
        property' = property props

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

        a <~? b = b >>= \case
          Just x  -> assign a x
          Nothing -> return ()

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

parseObjectMap :: ObjectIfaceMap -> FillM ObjectMap
parseObjectMap = M.traverseWithKey parseObject

type ObjectIfaceMap = Map DBus.ObjectPath InterfaceMap

type ObjectMap = Map DBus.ObjectPath Object

type PropertyMap = Map String DBus.Variant

-- | A map from interface names to a property map
type InterfaceMap = Map String PropertyMap

type FillM = Except Text

property :: DBus.IsVariant a => PropertyMap -> String -> FillM a
property m k = case M.lookup k m of
  Just x -> maybeVariant x
  Nothing -> throwE $ notfound k

  where maybeVariant v = case DBus.fromVariant v of
          Nothing -> throwE $ conversion v
          Just v' -> return v'

        notfound :: String -> Text
        notfound p = "Expected property " <> T.pack p <> " but it wasn't there"
        conversion v = "Failed type conversion for: " <> T.pack (show v) <> " (Property "
                       <> T.pack k <> ")"

class FillIface i where
  fillIface :: PropertyMap -> FillM i
  ifaceName :: Proxy i -> String

instance FillIface BlockIface where
  fillIface = fillBlockIface
  ifaceName _ = "org.freedesktop.UDisks2.Block"

instance FillIface FileSystemIface where
  fillIface = fillFSIface
  ifaceName _ = "org.freedesktop.UDisks2.Filesystem"

instance FillIface PartititionIface where
  fillIface = fillPartititionIface
  ifaceName _ = "org.freedesktop.UDisks2.Partitition"

instance FillIface LoopIface where
  fillIface = fillLoopIface
  ifaceName _ =  "org.freedesktop.UDisks2.Loop"

interface :: forall i. FillIface i => InterfaceMap -> FillM (Maybe i)
interface ifaces = maybe (return Nothing) (fmap Just . fillIface) $ M.lookup key ifaces
  where key = ifaceName (Proxy :: Proxy i)

-- propertiesToBlockDevice :: PropMap -> Maybe PropMap -> Either String BlockDevice
-- propertiesToBlockDevice m fsMap = do
--   filesystem <- traverse fillFileSystem fsMap
--   fillDevice m filesystem

-- fillFileSystem :: PropMap -> Either String FileSystem
-- fillFileSystem m =
--   FileSystem <$> fmap decodeUtf8 <$> property m "MountPoints"

-- fillDevice :: PropMap -> Maybe FileSystem -> Either String BlockDevice
-- fillDevice m isFileSystem = do
--   -- NOTE We assume here, that the filesystem paths are encoded in utf8
--   -- This may of course be violated on some systems, but those may not
--   -- be worthwhile to support for a udisks helper program
--   device <- decodeUtf8 <$> property' "Device"
--   preferredDevice <- decodeUtf8 <$> property' "PreferredDevice"
--   symlinks <- fmap decodeUtf8 <$> property' "Symlinks"
--   deviceNumber <- property' "DeviceNumber"
--   deviceId <- property' "Id"
--   size <- property' "Size"
--   readOnly <- property' "ReadOnly"
--   drive <- Drive <$> property' "Drive"
--   idUsage <- property' "IdUsage"
--   idType <- property' "IdType"
--   idVersion <- property' "IdVersion"
--   idLabel <- property' "IdLabel"
--   idUUID <- property' "IdUUID"
--   hintPartititionable <- property' "HintPartitionable"
--   hintSystem <- property' "HintSystem"
--   hintIgnore <- property' "HintIgnore"
--   hintAuto <- property' "HintAuto"
--   hintName <- property' "HintName"
--   hintIconName <- property' "HintIconName"
--   hintSymbolicIconName <- property' "HintSymbolicIconName"

--   return BlockDev {..}

--   where property' :: DBus.IsVariant a => String -> Either String a
--         property' = property m

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8 . B.filter (not.nullbyte)
  where nullbyte = (==0)

maybeRoot :: DBus.ObjectPath -> Maybe DBus.ObjectPath
maybeRoot p
  | p == "/"  = Nothing
  | otherwise = Just p
