{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Main where

import Brick
import Brick.Widgets.DeviceList
import Brick.Widgets.Border
import Graphics.Vty hiding (Event, nextEvent)
import qualified Graphics.Vty as Vty

import DBus.UDisks2.Simple

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import System.Exit
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Default
import Data.Monoid
import Control.Monad.IO.Class

data AppState = AppState {
  devList :: List Device,
  message :: Text,
  connection :: Connection
}

data AppEvent = DBusEvent Event
              | VtyEvent Vty.Event

draw :: AppState -> [Widget]
draw (AppState dl msg _) = [w]
  where w =     renderDeviceList dl
            <=> hBorder
            <=> txt msg

handler :: AppState -> AppEvent -> (EventM (Next AppState))
handler appState@AppState{..} e = case e of
  VtyEvent e'@(EvKey _ _) ->
    return (clearMessage appState)  -- clear message on every keystroke
    >>= handleKey e'
  VtyEvent _ -> continue appState
  DBusEvent (DeviceAdded dev) ->
    continueWith $ onList (listAppend dev)
  DBusEvent (DeviceRemoved dev) ->
    continueWith $ onList (listRemoveEq dev)
  DBusEvent (DeviceChanged old new) ->
    continueWith $ onList (listSwap old new)

  where continueWith :: (AppState -> AppState) -> EventM (Next AppState)
        continueWith f = return (f appState) >>= continue

        handleKey (EvKey (KChar 'q') []) as = halt as
        handleKey (EvKey KEnter []) as =
          liftIO (mountUnmount as) >>= continue
        handleKey e as = do
          lst' <- handleHJKLEvent e devList
          continue $ as { devList = lst' }

theme :: AttrMap
theme = attrMap defAttr
  [ (listSelectedAttr, defAttr `withBackColor` brightBlack) ]

main :: IO ()
main = do
  (con,devs) <- connect >>= \case
    Left err -> do
      T.hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right x -> return x

  let devList = listMoveTo 0 $ newDeviceList "devices" devs
      app = App
            { appDraw = draw
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handler
            , appStartEvent = return
            , appAttrMap = const theme
            , appLiftVtyEvent = VtyEvent
            }

  eventChan <- newChan

  forkIO $ eventThread con eventChan

  void $ customMain (mkVty def) eventChan app $
    AppState devList "Welcome" con

eventThread :: Connection -> Chan AppEvent -> IO ()
eventThread con chan = forever $ do
  ev <- nextEvent con
  writeChan chan (DBusEvent ev)

mountUnmount :: AppState -> IO AppState
mountUnmount as@AppState{..} = case listSelectedElement devList of
  Nothing -> return $ showMessage as "No device selected"
  Just (_, dev)
    | devMounted dev  -> unmount connection dev >>= \case
        Left err -> return $ showMessage as $ "error: " <> err
        Right () -> return $ showMessage as $ "Device unmounted"
    | otherwise       -> mount connection dev >>= \case
        Left err -> return $ showMessage as $ "error: " <> err
        Right mp -> return $ showMessage as $ "Device mounted at " <> mp

showMessage :: AppState -> Text -> AppState
showMessage as msg = as { message = msg }

clearMessage :: AppState -> AppState
clearMessage = flip showMessage " "

-- not onLisp!
onList :: (List Device -> List Device) -> AppState -> AppState
onList f appState = appState { devList = f (devList appState)}
