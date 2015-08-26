{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}

module Main (main) where

-- import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.ColumnList
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.EventLoop
import Graphics.Vty.Widgets.Util
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Box
import Graphics.Vty.Widgets.Borders
import Graphics.Vty hiding (nextEvent)
import Control.Monad
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import qualified Data.Vector as V
import System.IO
import System.Exit

import DBus.UDisks2.Simple
import ErrorLogger

main :: IO ()
main = do
  let nameColumn =  devName
      devFileColumn = devFile
      mountPointCol = T.intercalate "," . V.toList . devMountPoints
      mountedColumn d = if devMounted d then "✔" else " "

  lst <- newList defAttr [ ColumnSpec "Mounted" (Fixed 7) mountedColumn
                         , ColumnSpec "Name" Expand nameColumn
                         , ColumnSpec "Device" Expand devFileColumn
                         , ColumnSpec "Mount point" Expand mountPointCol
                         ]

  statusBar <- plainText "Welcome"

  layout <- return lst <--> hBorder <--> return statusBar

  fg <- newFocusGroup
  addToFocusGroup fg layout

  c <- newCollection
  _ <- addToCollection c layout fg

  errLog <- ErrLog <$> newChan

  connect >>= \case
    Left err -> do
      T.hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right (con, devs) -> do
      fg `onKeyPressed` \_ key _ ->
          if key == KChar 'q' then
          shutdownUi >> return True
          else return False

      lst `onItemActivated` \(ActivateItemEvent _ dev) ->
        if devMounted dev
          then void $ unmount con dev
          else void $ mount con dev -- TODO log errors

      forM_ devs $ \d -> addDevice lst d

      void $ forkIO $ eventThread lst con
      void $ forkIO $ logThread errLog statusBar

      runUi c defaultContext { focusAttr = black `on` yellow }

type ListWidget = Widget (ColumnList Device)

eventThread :: ListWidget -> Connection -> IO ()
eventThread list con = forever $ do
  event <- nextEvent con
  case event of
    DeviceRemoved path -> remDevice list path
    DeviceAdded   dev  -> addDevice list dev
    DeviceChanged old new  -> changeDevice list old new

addDevice :: ListWidget -> Device -> IO ()
addDevice lst d = schedule $ addToList lst d

remDevice :: ListWidget -> Device -> IO ()
remDevice lst path = do
  idx <- getIndex' lst path
  case idx of
    Just idx' -> schedule $ removeFromList lst idx' >> return ()
    Nothing   -> return ()

changeDevice :: ListWidget -> Device -> Device -> IO ()
changeDevice lst old new = do
  idx <- getIndex lst old
  remDevice lst old
  case idx of
    Just idx' -> schedule $ insertIntoList lst new idx'
    Nothing   -> addDevice lst new

getIndex :: ListWidget -> Device -> IO (Maybe Int)
getIndex lst dev = getIndex' lst dev

getIndex' :: ListWidget -> Device -> IO (Maybe Int)
getIndex' lst dev1 = do
  indices <- getIndices lst
  return $ fst <$> find (\(_, dev2) -> dev2 == dev1) indices

getIndices :: ListWidget -> IO [(Int, Device)]
getIndices lst = do
  count <- getListSize lst
  forM [0..count-1] $ \i -> do
    Just dev <- getListItem lst i
    return (i, dev)


newtype ErrLog = ErrLog (Chan T.Text)

instance ErrorLogger ErrLog where
  logError (ErrLog chan) t = writeChan chan t

logThread :: ErrLog -> Widget FormattedText -> IO ()
logThread (ErrLog chan) statusBar = forever $ do
  msg <- readChan chan
  setText statusBar msg
