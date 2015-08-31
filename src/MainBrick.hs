{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Brick
import Brick.Widgets.DeviceList
import Brick.Widgets.Border
import Graphics.Vty hiding (Event)
import qualified Graphics.Vty as Vty

import DBus.UDisks2.Simple

import qualified Data.Text.IO as T
import Data.Text (Text)
import System.Exit
import System.IO
import Control.Monad

data AppState = AppState {
  devList :: List Device,
  message :: Text
}

draw :: AppState -> [Widget]
draw (AppState dl msg) = [w]
  where w =     renderDeviceList dl
            <=> hBorder
            <=> txt msg

handler :: AppState -> Vty.Event -> (EventM (Next AppState))
handler appState e = case e of
  EvKey (KChar 'q') [] -> halt appState

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
            , appLiftVtyEvent = id
            }

  void $ defaultMain app (AppState devList "Welcome")
