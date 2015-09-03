{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module DBus.UDisks2.Types where

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
import Data.String
import Data.Int
import Data.Maybe

import Control.Monad.Trans.Except
import Control.Monad.State

import qualified DBus
import Control.Lens.TH
import Control.Lens (assign, Lens', ASetter, use, (^.))

-- TODO This module contains a lot of duplication.
--
-- For example, we always have two functions fill* and change* that do almost
-- the same. The first one initially creates an object, the second one changes
-- some properties on it.
--
-- The reason for this is that we get warnings if we forgot to initialize
-- something, but not if we only failed to change something. So if we reduce the
-- first case to the second, we would lose warnings.

newtype ObjectId = ObjectId DBus.ObjectPath
                 deriving (Show, Eq, Ord, IsString)

-- Used for the IdType property of the Block interface
data IdType = IdFilesystem | IdCrypto | IdRaid | IdOther Text
            deriving (Show)

-- TODO Better type than DBus.Variant for the configuration value
newtype Configuration = Configuration (Vector (Text, (Map Text DBus.Variant)))
                      deriving (Show)

data BlockIface = BlockIface {
  _blockObj :: ObjectId,
  _blockDevice :: Text,
  _blockPreferredDevice :: Text,
  _blockSymlinks :: Vector Text,
  _blockDeviceNumber :: Word64,
  _blockDeviceId :: Text,
  _blockSize :: Word64,
  _blockReadOnly :: Bool,
  _blockDrive :: Maybe ObjectId, -- "/" means Nothing
  _blockMdRaid :: Maybe ObjectId, -- "/" means Nothing
  _blockMdRaidMember :: Maybe ObjectId, -- "/" means Nothing
  _blockIdUsage :: Text,
  _blockIdType :: IdType,
  _blockIdVersion :: Text,
  _blockIdLabel :: Text,
  _blockIdUUID :: Text,
  _blockConfiguration :: Configuration,
  _blockCryptoBackingDevice :: Maybe ObjectId,
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
  _fsObj :: ObjectId,
  _fsMountPoints :: Vector Text
} deriving (Show)

makeLenses ''FileSystemIface

data PartititionIface = PartititionIface {
  _partititionObj :: ObjectId,
  _partititionNumber :: Word32,
  _partititionPartititionType :: Text,
  _partititionFlags :: Word64,
  _partititionOffset :: Word64,
  _partititionSize :: Word64,
  _partititionName :: Text,
  _partititionUUID :: Text,
  _partititionTable :: ObjectId,
  _partititionIsContainer :: Bool,
  _partititionIsContained :: Bool
} deriving (Show)

makeLenses ''PartititionIface

data LoopIface = LoopIface {
  _loopObj :: ObjectId,
  _loopBackingFile :: Text,
  _loopAutoclear :: Bool,
  _loopSetupByUID :: Word32
} deriving (Show)

makeLenses ''LoopIface

data BlockDevice = BlockDevice {
  _blockDevObj :: ObjectId,
  _blockDevBlock :: BlockIface,
  _blockDevFS :: Maybe FileSystemIface,
  _blockDevPartitition :: Maybe PartititionIface,
  _blockDevLoop :: Maybe LoopIface
} deriving (Show)

makeLenses ''BlockDevice

data Media = NoMedia
           | Thumb
           | Flash
           | FlashCF
           | FlashMS
           | FlashSM
           | FlashSD
           | FlashSdhc
           | FlashSdxc
           | FlashMmc
           | Floppy
           | FloppyZip
           | FloppyJaz
           | Optical
           | OpticalCd
           | OpticalCdR
           | OpticalCdRw
           | OpticalDvd
           | OpticalDvdR
           | OpticalDvdRw
           | OpticalDvdRam
           | OpticalDvdPlusR
           | OpticalDvdPlusRw
           | OpticalDvdPlusRDl
           | OpticalDvdPlusRwDl
           | OpticalBd
           | OpticalBdR
           | OpticalBdRe
           | OpticalHddvd
           | OpticalHddvdR
           | OpticalHddvdRw
           | OpticalMo
           | OpticalMrw
           | OpticalMrwW
           | Other Text
           deriving (Show,Eq,Ord)

data DriveIface = DriveIface {
  _driveIObj :: ObjectId,
  _driveIVendor :: Text,
  _driveIModel :: Text,
  _driveIRevision :: Text,
  _driveISerial :: Text,
  _driveIWwn :: Text,
  _driveIId :: Text,
  _driveIConfiguration :: Map Text DBus.Variant,
  _driveIMedia :: Media,
  _driveIMediaCompatibility :: Vector Media,
  _driveIMediaRemovable :: Bool,
  _driveIMediaAvailable :: Bool,
  _driveIMediaChangeDetected :: Bool,
  _driveISize :: Word64,
  _driveITimeDetected :: Word64,
  _driveITimeMediaDetected :: Word64,
  _driveIOptical :: Bool,
  _driveIOpticalBlank :: Bool,
  _driveIOpticalNumTracks :: Word32,
  _driveIOpticalNumAudioTracks :: Word32,
  _driveIOpticalNumDataTracks  :: Word32,
  _driveIOpticalNumSessions :: Word32,
  _driveIRotationRate :: Int32,
  _driveIConnectionBus :: Text,
  _driveISeat :: Text,
  _driveIRemovable :: Bool,
  _driveIEjectable :: Bool,
  _driveISortKey :: Text,
  _driveICanPowerOff :: Bool,
  _driveISiblingId :: Text
} deriving (Show)

makeLenses ''DriveIface

data AtaIface = AtaIface {
  _ataObj :: ObjectId,
  _ataSmartSupported :: Bool,
  _ataSmartEnabled :: Bool,
  _ataSmartUpdated :: Word64,
  _ataSmartFailing :: Bool,
  _ataSmartPowerOnSeconds :: Word64,
  _ataSmartTemperature :: Double,
  _ataSmartNumAttributesFailing :: Int32,
  _ataSmartNumAttributesFailedInThePast :: Int32,
  _ataSmartNumBadSectors :: Int64,
  _ataSmartSelftestStatus :: Text,
  _ataSmartSelftestPercentRemaining :: Int32,
  _ataPmSupported :: Bool,
  _ataPmEnabled :: Bool,
  _ataApmSupported :: Bool,
  _ataApmEnabled :: Bool,
  _ataAamSupported :: Bool,
  _ataAamEnabled :: Bool,
  _ataAamVendorRecommendedValue :: Int32,
  _ataWriteCacheSupported :: Bool,
  _ataWriteCacheEnabled :: Bool,
  _ataSecurityEraseUnitMinutes :: Int32,
  _ataSecurityEnhancedEraseUnitMinutes :: Int32,
  _ataSecurityFrozen :: Bool
} deriving (Show)

makeLenses ''AtaIface

data Drive = Drive {
  _driveObj :: ObjectId,
  _driveDrive :: DriveIface,
  _driveAta :: Maybe AtaIface
} deriving (Show)

makeLenses ''Drive

data Object = BlockDevObject BlockDevice
            | DriveObject Drive
            | MDRaidObject
            | JobObject
            | OtherObject ObjectId
            deriving (Show)

makePrisms ''Object
