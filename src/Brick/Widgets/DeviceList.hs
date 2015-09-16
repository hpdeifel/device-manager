{-# LANGUAGE OverloadedStrings #-}

module Brick.Widgets.DeviceList
       ( newDeviceList
       , renderDeviceList
       , module Brick.Widgets.List
       , module Brick.Widgets.List.Utils
       ) where

import Brick
import Brick.Widgets.List
import Brick.Widgets.List.Utils
import Brick.Widgets.Border
import DBus.UDisks2.Simple

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Bool
import Control.Lens
import Data.Monoid
import Data.Word
import Data.Maybe
import Text.Printf
import Data.List

newDeviceList :: Name -> [Device] -> List Device
newDeviceList name devs = list name (V.fromList devs) itemHeight
  where itemHeight = 1

renderDeviceList :: List Device -> Widget
renderDeviceList list = Widget Fixed Greedy $ do
  let devs = listElements list

  c <- getContext

  let availableWidth = c ^. availWidthL

      mountedWidth = minColumnWidth "Mounted" mountedColumn devs
      notMountedWidth = positive $ availableWidth - mountedWidth

      [nameWidth, fileWidth, mountPointWidth]
        = expand notMountedWidth
                 [ minColumnWidth "Device" nameColumn devs
                 , minColumnWidth "File" devFileColumn devs
                 , minColumnWidth "Mount Point" mountPointColumn devs
                 ]

      renderRow selected dev = selAttr selected $
                 hFix mountedWidth (txt $ mountedColumn dev)
             <+> hFix nameWidth (txt $ nameColumn dev)
             <+> hFix fileWidth (txt $ devFileColumn dev)
             <+> hFix mountPointWidth (txt $ mountPointColumn dev)

      selAttr True = withAttr listSelectedAttr
      selAttr False = id

      renderHeader =
                 hFix mountedWidth (txt "Mounted")
             <+> hFix nameWidth (txt "Device")
             <+> hFix fileWidth (txt "File")
             <+> hFix mountPointWidth (txt "Mount Point")

  render $ renderHeader <=> hBorder <=> renderList list renderRow

-- We substract 1 from the width, to guarantee at least one character of space
-- between two columns.
hFix :: Int -> Widget -> Widget
hFix width = (<+> txt " ") -- Add space for column separation
           . hLimit (positive $ width-1) -- Limit original widget to (width-1) chars
           . padRight Max -- Expand original widget infinitly to the right

nameColumn :: Device -> Text
nameColumn dev
  | devMediaType dev `elem` knownSizeDevices
    = formatType dev <> pad (optionally (devName dev))
  | otherwise
    = padR (formatSize dev) <> formatType dev <> pad (optionally (devName dev))

  where knownSizeDevices =
           [ OpticalCd
           , OpticalCdR
           , OpticalCdRw
           , OpticalDvd
           , OpticalDvdR
           , OpticalDvdRw
           , OpticalDvdPlusR
           , OpticalDvdPlusRw
           , OpticalBd
           , OpticalBdR
           , OpticalBdRe
           ]

formatSize :: Device -> Text
formatSize dev = si (devSize dev)

formatType :: Device -> Text
formatType dev = case devMediaType dev of
  Thumb -> "Thumb drive"
  Flash -> "Flash drive"
  FlashSD -> "SD Card"
  FlashMmc -> "MMC Card"
  Floppy -> "Floppy"
  OpticalCd -> "CD"
  OpticalCdR -> "CD-R"
  OpticalCdRw -> "CD-RW"
  OpticalDvd -> "DVD"
  OpticalDvdR -> "DVD-R"
  OpticalDvdRw -> "DVD-RW"
  OpticalDvdPlusR -> "DVD+R"
  OpticalDvdPlusRw -> "DVD+RW"
  OpticalBd -> "Blu-ray"
  OpticalBdR -> "Blu-ray" -- TODO Better name
  OpticalBdRe -> "Blu-ray" -- TODO Better name
  _ -> "Volume"

si :: Word64 -> Text
si size = T.pack $ printf "%.*f %s" (precision fitSize) fitSize fitUnit
  where sizes = iterate (/1000.0) (fromIntegral size) :: [Double]
        units = ["B", "kB", "MB", "GB", "TB"] :: [String]
        both  = zip sizes units

        (fitSize, fitUnit) = fromMaybe (last both) $ find ((<1000.0) . fst) both

        precision :: Double -> Int
        precision s  -- Show one digit after decimal point if (<10)
          | s < 10    = 1
          | otherwise = 0

optionally :: Text -> Text
optionally t
  | T.null t  = ""
  | otherwise = "(" <> t <> ")"

-- Add a character in front if the text is not empty
pad :: Text -> Text
pad t
  | T.null t = t
  | otherwise = " " <> t

-- Add a character at tail if the text is not empty
padR :: Text -> Text
padR t
  | T.null t = t
  | otherwise = t <> " "

devFileColumn :: Device -> Text
devFileColumn = padWhenEmpty . devFile

mountPointColumn :: Device -> Text
mountPointColumn dev = padWhenEmpty $
  T.intercalate "," $ V.toList $ devMountPoints dev

mountedColumn :: Device -> Text
mountedColumn = bool " " "✔" . devMounted

minColumnWidth :: Text -> (Device -> Text) -> Vector Device -> Int
minColumnWidth header content devs = max maxWidth (T.length header) + 2
  where maxWidth
          | V.null devs  = 0
          | otherwise = V.maximum $ V.map (T.length . content) devs

padWhenEmpty :: Text -> Text
padWhenEmpty t
  | T.null t  = " "
  | otherwise = t

positive :: Int -> Int
positive = max 0

-- Distributes the free space over the individual elements
-- TODO Allow overlong columns to steal width from the others
expand :: Int -> [Int] -> [Int]
expand total elems = elems'
  where part = total `div` num
        remaining = total `rem` num
        num = length elems
        elems' = zipWith (+)
                  (replicate remaining 1 ++ repeat 0)
                  (replicate num part)

toZero :: Int -> [Int]
toZero 0 = repeat 0
toZero n = n : toZero (n-1)
