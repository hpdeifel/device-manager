{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module DBus.UDisks.Types
       ( BlockDevice(..)
       , FileSystem(..)
       , Drive(..)
       , propertiesToBlockDevice
       ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import Data.Word
import qualified Data.Map as M
import Data.Map (Map)

import qualified DBus

data BlockDevice = BlockDev {
  device :: Text,
  preferredDevice :: Text,
  symlinks :: Vector Text,
  deviceNumber :: Word64,
  deviceId :: Text,
  size :: Word64,
  readOnly :: Bool,
  drive :: Drive,
  -- TODO MDRaid
  -- TODO MDRaidMember
  idUsage :: Text,
  idType :: Text,
  idVersion :: Text,
  idLabel :: Text,
  idUUID :: Text,
  -- TODO Configuration
  -- TODO CryptoBackingDevice
  hintPartititionable :: Bool,
  hintSystem :: Bool,
  hintIgnore :: Bool,
  hintAuto :: Bool,
  hintName :: Text,
  hintIconName :: Text,
  hintSymbolicIconName :: Text,
  isFileSystem :: Maybe FileSystem
} deriving (Show)

data FileSystem = FileSystem {
  mountPoints :: Vector Text
} deriving (Show)

-- TODO More information in drive
data Drive = Drive DBus.ObjectPath
           deriving (Show)

type PropMap = Map String DBus.Variant

propertiesToBlockDevice :: PropMap -> Maybe PropMap -> Either String BlockDevice
propertiesToBlockDevice m fsMap = do
  filesystem <- traverse fillFileSystem fsMap
  fillDevice m filesystem

fillFileSystem :: PropMap -> Either String FileSystem
fillFileSystem m =
  FileSystem <$> fmap decodeUtf8 <$> property m "MountPoints"

fillDevice :: PropMap -> Maybe FileSystem -> Either String BlockDevice
fillDevice m isFileSystem = do
  -- NOTE We assume here, that the filesystem paths are encoded in utf8
  -- This may of course be violated on some systems, but those may not
  -- be worthwhile to support for a udisks helper program
  device <- decodeUtf8 <$> property' "Device"
  preferredDevice <- decodeUtf8 <$> property' "PreferredDevice"
  symlinks <- fmap decodeUtf8 <$> property' "Symlinks"
  deviceNumber <- property' "DeviceNumber"
  deviceId <- property' "Id"
  size <- property' "Size"
  readOnly <- property' "ReadOnly"
  drive <- Drive <$> property' "Drive"
  idUsage <- property' "IdUsage"
  idType <- property' "IdType"
  idVersion <- property' "IdVersion"
  idLabel <- property' "IdLabel"
  idUUID <- property' "IdUUID"
  hintPartititionable <- property' "HintPartitionable"
  hintSystem <- property' "HintSystem"
  hintIgnore <- property' "HintIgnore"
  hintAuto <- property' "HintAuto"
  hintName <- property' "HintName"
  hintIconName <- property' "HintIconName"
  hintSymbolicIconName <- property' "HintSymbolicIconName"

  return BlockDev {..}

  where property' :: DBus.IsVariant a => String -> Either String a
        property' = property m

property :: DBus.IsVariant a => PropMap -> String -> Either String a
property m k = case M.lookup k m of
  Just x -> maybeVariant x
  Nothing -> Left $ notfound k

  where maybeVariant v = maybe (Left $ conversion v) Right $ DBus.fromVariant v

        notfound :: String -> String
        notfound p = "Expected property " ++ p ++ " in device, but it wasn't there"
        conversion v = "Failed type conversion for: " ++ show v

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8 . B.filter (not.nullbyte)
  where nullbyte = (==0)
