{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}

module Main where

import DBus
import DBus.Client
import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.Word (Word32)
import Data.Int (Int32)
import Control.Exception.Base
import System.Process
import qualified Data.Text.IO as T
import System.IO

import DBus.DBusAbstraction
import DBus.UDisks
import Common
import ErrorLogger

data Notify = Notify Client

instance DBusObject Notify where
  getObjectPath _  = "/org/freedesktop/Notifications"
  getInterface _   = "org.freedesktop.Notifications"
  getDestination _ = "org.freedesktop.Notifications"

data NoteData = Data (M.Map ObjectPath Device) (M.Map Word32 ObjectPath)

data StderrLogger = StderrLogger

instance ErrorLogger StderrLogger where
  logError _ = T.hPutStrLn stderr

main :: IO ()
main = do
  client <- connectSession

  con <- udisksConnect StderrLogger

  chan <- newChan

  devs <- getDeviceList con
  let objMap = concat $ map checkDev devs
      checkDev d = if internal d || hasPartitions d
                 then [] else [(objectPath d, d)]
  var <- newMVar (Data (M.fromList $ objMap)
                       (M.fromList $ map (\d -> (0,objectPath d)) devs))

  void $ addMatch client (matchSignal (Notify client) "NotificationClosed")
    (closedCallback var)

  void $ addMatch client (matchSignal (Notify client) "ActionInvoked")
    (actionCallback con var)

  forM_ devs $ \d -> do
    listenDevice con d chan
  listenEvents con chan

  flip finally (disconnect client >> udisksDisconnect con) $
    forever $ do
      event <- readChan chan
      case event of
        DeviceAdded d -> do
          Data pathM idM <- takeMVar var

          unless (deviceBoring d) $ do
            iD <- addNotification client d
            putMVar var $ Data (M.insert (objectPath d) d pathM)
                               (case iD of
                                 Just iD' -> M.insert iD' (objectPath d) idM
                                 Nothing -> idM)

          -- also add boring devices, because they could become
          -- interesting in the future
          when (deviceBoring d) $
            putMVar var $ Data (M.insert (objectPath d) d pathM) idM

          listenDevice con d chan


        DeviceRemoved path -> do
          Data pathM idM <- takeMVar var
          let dev = M.lookup path pathM
              body = case dev of
                Just d -> mkBody d
                Nothing -> ""

          putMVar var $ Data (M.delete path pathM) idM
          when (isJust dev && (not $ deviceBoring $ fromJust dev)) $ do
            _ <- notify client "Device removed" body []
            return ()

        DeviceChanged dev -> do
          Data pathM idM <- takeMVar var

          let def = Data (M.insert (objectPath dev) dev pathM) idM

          new <- case M.lookup (objectPath dev) pathM of
            Just oldDev
              -- device was mounted and isn't now => was unmounted
              | isMounted oldDev && (not $ isMounted dev) -> do
                _ <- notify client "Device unmounted" (mkBody dev) []
                return def

              -- hadn't had media and now has => media became available
              | (not $ hasMedia oldDev) && (hasMedia dev) -> do
                iD <- addNotification client dev
                return $ Data (M.insert (objectPath dev) dev pathM)
                              (case iD of
                                Just iD' -> (M.insert iD' (objectPath dev) idM)
                                Nothing -> idM)
            _ -> return def

          putMVar var new

addNotification :: Client -> Device -> IO (Maybe Word32)
addNotification client dev =
  notify client "Device Added" (mkBody dev) ["mount", "Mount", "open", "Open"]


mkBody :: Device -> String
mkBody d = if name d == "" then deviceFile d
           else name d ++ " (" ++ deviceFile d ++ ")"

closedCallback :: MVar NoteData -> Signal -> IO ()
closedCallback var sig = do
  let iD = fromVariant' $ signalBody sig !! 0

  modifyMVar_ var $ \(Data pathM idM) ->
    return $ Data pathM (M.delete iD idM)

actionCallback :: ErrorLogger a => UDisksConnection a -> MVar NoteData -> Signal -> IO ()
actionCallback con var sig = do
  Data pathM idM <- takeMVar var

  let iD = fromVariant' $ signalBody sig !! 0
      action = fromVariant' $ signalBody sig !! 1 :: String

  let dev = M.lookup iD idM >>= flip M.lookup pathM

  flip (maybe (return ())) dev $ \d -> case action of
      "mount" -> doMount con d
      "open"  -> do
        doMount con d
        _ <- forkIO $ doOpen var (objectPath d)
        return ()
      _       -> return ()

  putMVar var $ Data pathM idM

doOpen :: MVar NoteData -> ObjectPath -> IO ()
doOpen var path = do
  let loop = do
        threadDelay 1000000
        Data pathMap _ <- readMVar var
        case M.lookup path pathMap >>= mountPoints of
          Just (mp:_) -> do
            _ <- runProcess "xdg-open" [mp] Nothing Nothing
                 Nothing Nothing Nothing
            return False
          _           -> return True

  foldM_ (\g _ -> if g then loop else return g) True [1..5::Int]

notify :: Client -> String -> String -> [String] -> IO (Maybe Word32)
notify client title body actions = do
  iD <- invoke client (Notify client) "Notify" [
    toVariant ("Device Notifier" :: String),
    toVariant (0 :: Word32),
    toVariant ("save" :: String),
    toVariant title,
    toVariant body,
    toVariant actions,
    toVariant (M.empty :: M.Map String Variant),
    toVariant (5000 :: Int32)
    ]

  case iD of
    Left err -> hPutStrLn stderr err >> return Nothing
    Right iD' -> return $ Just $ fromVariant' iD'
