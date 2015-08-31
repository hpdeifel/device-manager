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

hFix :: Int -> Widget -> Widget
hFix width = hLimit width . padRight Max

nameColumn :: Device -> Text
nameColumn dev = formatSize dev <> optionally (devName dev)

formatSize :: Device -> Text
formatSize dev = si (devSize dev)

si :: Word64 -> Text
si size = T.pack $ printf "%.1f %s" fitSize fitUnit
  where sizes = iterate (/1000.0) (fromIntegral size) :: [Double]
        units = ["B", "kB", "MB", "GB", "TB"] :: [String]
        both  = zip sizes units

        (fitSize, fitUnit) = fromMaybe (last both) $ find ((<1000.0) . fst) both

optionally :: Text -> Text
optionally t
  | T.null t  = ""
  | otherwise = " (" <> t <> ")"

devFileColumn :: Device -> Text
devFileColumn = padWhenEmpty . devFile

mountPointColumn :: Device -> Text
mountPointColumn dev = padWhenEmpty $
  T.intercalate "," $ V.toList $ devMountPoints dev

mountedColumn :: Device -> Text
mountedColumn = bool " " "✔" . devMounted

minColumnWidth :: Text -> (Device -> Text) -> (Vector Device) -> Int
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
expand :: Int -> [Int] -> [Int]
expand total elems = elems'
  where remaining = total - sum elems
        num       = length elems
        elems'    = zipWith (+)
                      (toZero (remaining `rem` num))
                      (map (+(remaining `div` num)) elems)


toZero :: Int -> [Int]
toZero 0 = repeat 0
toZero n = n : toZero (n-1)
