{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, LambdaCase #-}

module Main where

import qualified DBus
import qualified DBus.Client as DBus
import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.Word (Word32)
import Data.Int (Int32)
import Data.List
import Data.Monoid
import Data.Vector ((!?))
import Control.Exception.Base
import System.Process
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import System.IO

import DBus.DBusAbstraction
import DBus.UDisks2.Simple

data Notify = Notify DBus.Client

instance DBusObject Notify where
  type DefaultInterface Notify = Notify
  getObjectPath _  = "/org/freedesktop/Notifications"
  getDestination _ = "org.freedesktop.Notifications"

instance DBusInterface Notify where
  getInterface _   = "org.freedesktop.Notifications"

instance Implements Notify Notify

data NoteData = Data [Device] (M.Map Word32 Device)

main :: IO ()
main = do
  client <- DBus.connectSession

  (con, devs) <- connect >>= \case
    Left err -> error (show err)
    Right x -> return x

  chan <- newChan

  var <- newMVar (Data devs M.empty)

  void $ DBus.addMatch client (matchSignal (Notify client) "NotificationClosed")
    (closedCallback var)

  void $ DBus.addMatch client (matchSignal (Notify client) "ActionInvoked")
    (actionCallback con var)

  flip finally (DBus.disconnect client >> disconnect con) $
    forever $ do
      event <- nextEvent con
      case event of
        DeviceAdded d -> do
          Data devs idMap <- takeMVar var

          iD <- addNotification client d
          let devs' = d : delete d devs
          putMVar var $ Data devs' (maybe idMap (\x -> M.insert x d idMap) iD)


        DeviceRemoved dev -> do
          Data devs idMap <- takeMVar var
          let body = mkBody dev

          putMVar var $ Data (delete dev devs) idMap
          void $ notify client "Device removed" body []

        DeviceChanged old new -> do
          Data devs idMap <- takeMVar var


          when (devMounted old && (not $ devMounted new)) $ do
            void $ notify client "Device unmounted" (mkBody new) []

          putMVar var $ Data (new : delete old devs) idMap

addNotification :: DBus.Client -> Device -> IO (Maybe Word32)
addNotification client dev =
  notify client "Device Added" (mkBody dev) ["mount", "Mount", "open", "Open"]


mkBody :: Device -> Text
mkBody d = if devName d == "" then devFile d
           else devName d <> " (" <> devFile d <> ")"

closedCallback :: MVar NoteData -> DBus.Signal -> IO ()
closedCallback var sig = do
  let iD = fromVariant' $ DBus.signalBody sig !! 0

  modifyMVar_ var $ \(Data pathM idM) ->
    return $ Data pathM (M.delete iD idM)

actionCallback :: Connection -> MVar NoteData -> DBus.Signal -> IO ()
actionCallback con var sig = do
  Data devs idM <- takeMVar var

  let iD = fromVariant' $ DBus.signalBody sig !! 0
      action = fromVariant' $ DBus.signalBody sig !! 1 :: String

  let dev = M.lookup iD idM

  flip (maybe (return ())) dev $ \d -> case action of
      "mount" -> void $ mount con d
      "open"  -> mount con d >>= \case
        Left err -> return () -- TODO Log error
        Right mountPoint  -> void $ forkIO $ doOpen mountPoint
      _       -> return ()

  putMVar var $ Data devs idM

doOpen :: Text -> IO ()
doOpen mountPoint = do
  void $ runProcess "xdg-open" [T.unpack mountPoint] Nothing Nothing
    Nothing Nothing Nothing

notify :: DBus.Client -> Text -> Text -> [Text] -> IO (Maybe Word32)
notify client title body actions = do
  iD <- invoke client (Notify client) "Notify" [
    DBus.toVariant ("Device Notifier" :: String),
    DBus.toVariant (0 :: Word32),
    DBus.toVariant ("save" :: String),
    DBus.toVariant title,
    DBus.toVariant body,
    DBus.toVariant actions,
    DBus.toVariant (M.empty :: M.Map String DBus.Variant),
    DBus.toVariant (5000 :: Int32)
    ]

  case iD of
    Left err -> hPutStrLn stderr (show err) >> return Nothing
    Right iD' -> return $ Just $ fromVariant' iD'
