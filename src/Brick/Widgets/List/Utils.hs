module Brick.Widgets.List.Utils where

import Brick
import Graphics.Vty
import Brick.Widgets.List
import Control.Lens
import qualified Data.Vector as V

listAppend :: e -> List e -> List e
listAppend e l = listInsert (l^.listElementsL.to V.length) e l

listRemoveEq :: Eq e => e -> List e -> List e
listRemoveEq e l = case l^.listElementsL.to (V.elemIndex e) of
  Nothing -> l
  Just i  -> listRemove i l

listSwap :: Eq e => e -> e -> List e -> List e
listSwap old new l = case l^.listElementsL.to (V.elemIndex old) of
  Nothing -> listAppend new l
  Just i  -> listMoveTo i $ listInsert i new $ listRemove i l

handleHJKLEvent :: Event -> List e -> EventM (List e)
handleHJKLEvent ev lst = case ev of
  EvKey (KChar 'j') [] -> return $ listMoveDown lst
  EvKey (KChar 'k') [] -> return $ listMoveUp lst
  EvKey (KChar 'g') [] -> return $ listMoveTo 0 lst
  EvKey (KChar 'G') [] -> return $ listMoveTo (lst^.listElementsL.to V.length) lst
  _                    -> handleEvent ev lst
