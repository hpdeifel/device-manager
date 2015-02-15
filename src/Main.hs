{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

-- import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.ColumnList
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.EventLoop
import Graphics.Vty.Widgets.Util
import Graphics.Vty
import Control.Monad
import Control.Applicative ((<$>))
import Control.Concurrent
import qualified Data.Text as T
import Data.List
import Data.Maybe (isJust)

import DBus.UDisks
import Common

main :: IO ()
main = do
  let nameColumn = maybe "No name" T.pack . formatDeviceLabel
      devFileColumn = T.pack . deviceFile
      mountPointCol = maybe "" (T.pack . intercalate ",") . mountPoints
      mountedColumn d = if isMounted d then "✔" else " "

  lst <- newList defAttr [ ColumnSpec "Mounted" (Fixed 7) mountedColumn
                         , ColumnSpec "Name" Expand nameColumn
                         , ColumnSpec "Device" Expand devFileColumn
                         , ColumnSpec "Mount point" Expand mountPointCol
                         ]

  layout <- return lst

  fg <- newFocusGroup
  addToFocusGroup fg layout

  c <- newCollection
  _ <- addToCollection c layout fg

  con  <- udisksConnect
  devs <- getDeviceList con

  fg `onKeyPressed` \_ key _ ->
    if key == KChar 'q' then
      shutdownUi >> return True
      else return False

  lst `onItemActivated` \(ActivateItemEvent _ dev) -> do
    if (isJust $ mountPoints dev)
      then doUnmount con dev
      else doMount con dev

  chan <- newChan

  forM_ devs $ \d -> do
    addDevice lst d
    listenDevice con d chan

  listenEvents con chan

  forkIO $ eventThread lst con chan

  runUi c defaultContext { focusAttr = black `on` yellow }

type ListWidget = Widget (ColumnList Device)

eventThread :: ListWidget -> UDisksConnection -> Chan UDiskMessage -> IO ()
eventThread list con chan = forever $ do
  msg <- readChan chan
  case msg of
    DeviceRemoved path -> remDevice list path
    DeviceAdded   dev  -> addDevice list dev >> listenDevice con dev chan
    DeviceChanged dev  -> changeDevice list dev

addDevice :: ListWidget -> Device -> IO ()
addDevice lst d = unless (deviceBoring d) $
  schedule $ addToList lst d

remDevice :: ListWidget -> ObjectPath -> IO ()
remDevice lst path = do
  idx <- getIndex' lst path
  case idx of
    Just idx' -> schedule $ removeFromList lst idx' >> return ()
    Nothing   -> return ()

changeDevice :: ListWidget -> Device -> IO ()
changeDevice lst dev = do
  idx <- getIndex lst dev
  remDevice lst (objectPath dev)
  case idx of
    Just idx' -> unless (deviceBoring dev) $
      schedule $ insertIntoList lst dev idx'
    Nothing   -> addDevice lst dev

getIndex :: ListWidget -> Device -> IO (Maybe Int)
getIndex lst dev = getIndex' lst (objectPath dev)

getIndex' :: ListWidget -> ObjectPath -> IO (Maybe Int)
getIndex' lst path = do
  indices <- getIndices lst
  return $ fst <$> find (\(_, dev) -> objectPath dev == path) indices

getIndices :: ListWidget -> IO [(Int, Device)]
getIndices lst = do
  count <- getListSize lst
  forM [0..count-1] $ \i -> do
    Just dev <- getListItem lst i
    return (i, dev)
