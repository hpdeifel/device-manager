{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Graphics.Vty.Widgets.All
import Graphics.Vty
import System.Exit
import Control.Monad
import Control.Applicative ((<$>))
import Control.Concurrent
import qualified Data.Text as T
import Data.List
import Data.Maybe (isJust)

import UDisks

main :: IO ()
main = do
  title <- plainText "Devices:"
  lst <- newList def_attr

  layout <- return title <--> hBorder <--> return lst
  
  fg <- newFocusGroup
  addToFocusGroup fg layout
  
  c <- newCollection
  _ <- addToCollection c layout fg

  con  <- udisksConnect
  devs <- getDeviceList con

  fg `onKeyPressed` \_ key _ ->
    if key == KASCII 'q' then
      exitSuccess
      else return False

  lst `onItemActivated` \(ActivateItemEvent _ dev _) -> do
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

type ListWidget = Widget (List Device FormattedText) 

eventThread :: ListWidget -> UDisksConnection -> Chan UDiskMessage -> IO ()
eventThread list con chan = forever $ do
  msg <- readChan chan
  case msg of
    DeviceRemoved path -> remDevice list path
    DeviceAdded   dev  -> addDevice list dev >> listenDevice con dev chan
    DeviceChanged dev  -> changeDevice list dev

addDevice :: ListWidget -> Device -> IO ()
addDevice lst d = do
  unless (internal d || hasPartitions d) $ do
    widget <- deviceLine d
    schedule $ addToList lst d widget

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
    Just idx' -> unless (internal dev || hasPartitions dev) $ do
      widget <- deviceLine dev
      schedule $ insertIntoList lst dev widget idx'
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
    Just (dev, _) <- getListItem lst i
    return (i, dev)

deviceLine :: Device -> IO (Widget FormattedText)
deviceLine d = do
  let file  = deviceFile d
      label = name d
      mount = T.pack $ case mountPoints d of
        Just mp -> " mounted on " ++ intercalate ", " mp
        Nothing -> ""
      combined = (if label == "" then file else label ++ " (" ++ file ++ ")")
      text = T.pack combined

  widget <- plainText ""
  setTextWithAttrs widget [ (text,  def_attr)
                          , (mount, fgColor bright_green) 
                          ]
  return widget
