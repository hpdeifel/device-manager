{-# LANGUAGE OverloadedStrings #-}

module Brick.Widgets.DeviceList
       ( newDeviceList
       , renderDeviceList
       , module Brick.Widgets.List
       ) where

import Brick
import Brick.Widgets.List
import Brick.Widgets.Border
import DBus.UDisks2.Simple

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Bool
import Control.Lens

newDeviceList :: Name -> [Device] -> List Device
newDeviceList name devs = list name (V.fromList devs) itemHeight
  where itemHeight = 1

renderDeviceList :: List Device -> Widget
renderDeviceList list = Widget Fixed Fixed $ do
  let devs = listElements list

  c <- getContext

  let availableWidth = c ^. availWidthL

      mountedWidth = minColumnWidth "Mounted" mountedColumn devs
      notMountedWidth = positive $ availableWidth - mountedWidth

      [nameWidth, fileWidth, mountPointWidth]
        = expand notMountedWidth
                 [ minColumnWidth "Name" nameColumn devs
                 , minColumnWidth "Device" devFileColumn devs
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
             <+> hFix nameWidth (txt "Name")
             <+> hFix fileWidth (txt "Device")
             <+> hFix mountPointWidth (txt "Mount Point")

  render $ renderHeader <=> hBorder <=> renderList list renderRow

hFix :: Int -> Widget -> Widget
hFix width = hLimit width . padRight Max


nameColumn :: Device -> Text
nameColumn = devName

devFileColumn :: Device -> Text
devFileColumn = devFile

mountPointColumn :: Device -> Text
mountPointColumn dev
  | V.null (devMountPoints dev) = " "
  | otherwise = T.intercalate "," $ V.toList $ devMountPoints dev

mountedColumn :: Device -> Text
mountedColumn = bool " " "✔" . devMounted

minColumnWidth :: Text -> (Device -> Text) -> (Vector Device) -> Int
minColumnWidth header content devs = max maxWidth (T.length header) + 2
  where maxWidth
          | V.null devs  = 0
          | otherwise = V.maximum $ V.map (T.length . content) devs

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
