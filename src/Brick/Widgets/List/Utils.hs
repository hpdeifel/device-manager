module Brick.Widgets.List.Utils where

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
