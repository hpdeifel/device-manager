{-# LANGUAGE DeriveDataTypeable, MultiWayIf #-}

-- | Adapted from the vty-ui library by Jonathan Daugherty.
-- The original copyright was:
--
-- Copyright (c) 2009-2013, Jonathan Daugherty.
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * The names of the contributors may not be used to endorse or
--       promote products derived from this software without specific
--       prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Graphics.Vty.Widgets.ColumnList
       ( ColumnList
       , ColumnSpec(ColumnSpec)
       , ColumnType(..)
       , ListError(..)
       , NewItemEvent(..)
       , RemoveItemEvent(..)
       , SelectionEvent(..)
       , ActivateItemEvent(..)

       -- ** List creation
       , newList
       , addToList
       , insertIntoList
       , removeFromList
       -- ** List manipulation
       , scrollBy
       , scrollUp
       , scrollDown
       , pageUp
       , pageDown
       , onSelectionChange
       , onItemAdded
       , onItemRemoved
       , onItemActivated
       , activateCurrentItem
       , clearList
       , setSelected

       -- ** List inspection
       , getListSize
       , getSelected
       , getListItem
       ) where

import Graphics.Vty.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.Image
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Borders
import Graphics.Vty.Widgets.Fixed
import Graphics.Vty.Widgets.Events
import Graphics.Vty.Widgets.Util
import Graphics.Vty

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Text (Text)
import Data.List
import Data.Typeable

import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception

data ListError = BadItemIndex Int
               -- ^The specified position could not be used to remove
               -- an item from the list.
               | ResizeError
               | BadListWidgetSizePolicy
               -- ^The type of widgets added to the list grow
               -- vertically, which is not permitted.
                 deriving (Show, Typeable)

instance Exception ListError

data SelectionEvent a = SelectionOn Int a
                      -- ^An item at the specified position with the
                      -- specified internal value and widget was
                      -- selected.
                      | SelectionOff
                      -- ^No item was selected, which means the
                      -- list is empty.

-- |A new item was added to the list at the specified position with
-- the specified value and widget.
data NewItemEvent a = NewItemEvent Int a

-- |An item was removed from the list at the specified position with
-- the specified value and widget.
data RemoveItemEvent a = RemoveItemEvent Int a

-- |An item in the list was activated at the specified position with
-- the specified value and widget.
data ActivateItemEvent a = ActivateItemEvent Int a

data ColumnList a = ColumnList {
  listItems      :: V.Vector ([Widget FormattedText], a),
  columns        :: [ColumnSpec a],
  selectedUnfocusedAttr :: Attr,
  selectedIndex  :: Int,
  scrollTopIndex :: Int,
  headerWidgets  :: [Widget FormattedText],
  borderWidget   :: Widget HBorder,
  selectionChangeHandlers :: Handlers (SelectionEvent a),
  itemAddHandlers :: Handlers (NewItemEvent a),
  itemRemoveHandlers :: Handlers (RemoveItemEvent a),
  itemActivateHandlers :: Handlers (ActivateItemEvent a)
}

instance Show (ColumnList a) where
  show l = concat [ "ColumnList { "
                  , "columns = ", show $ columns l
                  , ", selectedUnfocusedAttr = ", show $ selectedUnfocusedAttr l
                  , ", selectedIndex = ", show $ selectedIndex l
                  , ", scrollTopIndex = ", show $ scrollTopIndex l
                  , ", listItems = <", show $ V.length $ listItems l, " items>"
                  , " }"
                  ]

data ColumnType = Expand | Fixed Int deriving (Show,Eq)

data ColumnSpec a = ColumnSpec {
  title    :: Text,
  colType  :: ColumnType,
  showFunc :: a -> Text
}

instance Show (ColumnSpec a) where
  show cs = concat [ "ColumnSpec { "
                   , "title = ", show $ title cs
                   , "type  = ", show $ colType cs
                   ]

newListData :: Attr -> [ColumnSpec a] -> IO (ColumnList a)
newListData selAttr cols = do
  header <- mapM (plainText . title) cols
  border <- hBorder
  schs <- newHandlers
  iahs <- newHandlers
  irhs <- newHandlers
  iacths <- newHandlers

  return $ ColumnList { listItems = V.empty
                      , columns = cols
                      , selectedUnfocusedAttr = selAttr
                      , selectedIndex = (-1)
                      , scrollTopIndex = 0
                      , headerWidgets = header
                      , borderWidget = border
                      , selectionChangeHandlers = schs
                      , itemAddHandlers = iahs
                      , itemRemoveHandlers = irhs
                      , itemActivateHandlers = iacths
                      }

renderList :: Widget (ColumnList a) -> DisplayRegion -> RenderContext -> IO Image
renderList w region context = do
  cl <- getState w
  coltypes <- map colType <$> columns <~~ w
  h <- renderLine (regionWidth region) context coltypes (headerWidgets cl)
  b <- render (borderWidget cl) region context
  height <- (fromIntegral . regionHeight) <$> getCurrentSize w
  foc <- focused <~ w

  let curScroll = scrollTopIndex cl
      items     = V.length (listItems cl) - 1
      visible = [curScroll..(min (curScroll + height) items)]
      defaultAttr = mergeAttrs [ overrideAttr context
                               , normalAttr context
                               ]

  body <- forM visible $ \i ->
    let line = fst (listItems cl ! i)
        att  = if foc then focusAttr context
               else mergeAttrs [ selectedUnfocusedAttr cl, defaultAttr ]
        ctx  = if (i == selectedIndex cl) then context { overrideAttr = att } else context
    in renderLine (regionWidth region) ctx coltypes line

  let size = (regionWidth region, regionHeight region)
      remaining = fromIntegral $ max 0 (fromIntegral (regionHeight region) - 2 - length visible)
      filler = if remaining == 0 then emptyImage
               else charFill (normalAttr context) ' ' (regionWidth region) remaining

  return $ crop (fst size) (snd size) $
    (h <-> b <-> foldr (<->) emptyImage body <-> filler)

renderLine :: Int -> RenderContext -> [ColumnType] -> [Widget FormattedText] -> IO Image
renderLine width' context colTypes cols = do
  imgs <- mapM rend $ zip widths cols
  return $ horizCat $ intersperse (backgroundFill 2 1) imgs

  where items = length cols
        width = fromIntegral width' - space
        space = (items - 1) * 2
        expandable = length $ filter (== Expand) colTypes
        totalFixed = sum $ map (\(Fixed n) -> n) $ filter (/= Expand) colTypes
        remainingWidth
          | totalFixed > width = 0
          | otherwise          = width - totalFixed
        minLengths = replicate expandable (remainingWidth `div` expandable)
        ones = replicate (remainingWidth `mod` expandable) 1
        expandWidths = zipWith (+) minLengths (ones ++ repeat 0)
        widths' [] _ = []
        widths' (Expand:rest) (e:es) = e : widths' rest es
        widths' (Fixed n:rest) es    = n : widths' rest es
        widths = widths' colTypes expandWidths
        mkRegion w = ((fromIntegral w), 1)
        rend (0, _) = return emptyImage
        rend (width, widget) = do
          fix <- hFixed width widget
          render fix (mkRegion width) context


newList :: Attr -> [ColumnSpec a] -> IO (Widget (ColumnList a))
newList selAttr cols = do
  list <- newListData selAttr cols
  wRef <- newWidget list $ \w ->
    w { render_ = renderList
      , growHorizontal_ = const $ return True
      , growVertical_ = const $ return True
      , getCursorPosition_ = const $ return Nothing
      , keyEventHandler = listKeyEvent
      }

  return wRef

listKeyEvent :: Widget (ColumnList a) -> Key -> [Modifier] -> IO Bool
listKeyEvent w KUp _ = scrollUp w >> return True
listKeyEvent w KDown _ = scrollDown w >> return True
listKeyEvent w KPageUp _ = pageUp w >> return True
listKeyEvent w KPageDown _ = pageDown w >> return True
listKeyEvent w KEnter _ = activateCurrentItem w >> return True
listKeyEvent _ _ _ = return False

addToList :: Widget (ColumnList a) -> a -> IO ()
addToList w element = do
  numItems <- (V.length . listItems) <~~ w
  insertIntoList w element numItems

insertIntoList :: Widget (ColumnList a) -> a -> Int -> IO ()
insertIntoList w element pos = do
  numItems <- (V.length . listItems) <~~ w
  oldSel <- selectedIndex <~~ w
  oldScr <- scrollTopIndex <~~ w
  cols <- columns <~~ w
  height <- (fromIntegral . regionHeight) <$> getCurrentSize w

  let newSelIndex
        | numItems == 0 = 0
        | pos <= oldSel = oldSel + 1
        | otherwise     = oldSel
      newScrollTop
        | pos <= oldSel && (oldSel - oldScr + 1) == height - 2 = oldScr + 1
        | pos <= oldSel = oldScr
        | otherwise     = oldScr

  widgets <- mapM (mkText element) cols

  let newItems s = if pos >= numItems
                   then V.snoc (listItems s) (widgets, element)
                   else vInject pos (widgets, element) (listItems s)

  updateWidgetState w $ \s -> s { listItems = newItems s
                                , selectedIndex = newSelIndex
                                , scrollTopIndex = newScrollTop
                                }

  notifyItemAddHandler w (min pos numItems) element

  when (oldSel /= newSelIndex) $
    notifySelectionHandler w

  where mkText e cs = plainText (showFunc cs e)
        vInject atPos a as = let (hd, t) = (V.take atPos as, V.drop atPos as)
                             in hd V.++ (V.cons a t)

removeFromList :: Widget (ColumnList a) -> Int -> IO a
removeFromList w pos = do
  items <- listItems <~~ w
  numItems <- (V.length . listItems) <~~ w
  oldSel <- selectedIndex <~~ w
  oldScr <- scrollTopIndex <~~ w

  when (pos < 0 || pos >= numItems) $
    throw $ BadItemIndex pos

  let (_,element) = items ! pos

      newScrollTop | oldScr == 0   = oldScr
                   | pos <  oldScr = oldScr - 1
                   | otherwise     = min oldScr newSelectedIndex

      newSelectedIndex
        | pos >  oldSel          = oldSel
        | pos <  oldSel          = min 0 (oldSel - 1)
        | oldSel == numItems - 1 = oldSel - 1
        | otherwise              = oldSel

  updateWidgetState w $ \s -> s { selectedIndex = newSelectedIndex
                                , listItems = V.take pos items V.++
                                              V.drop (pos + 1) items
                                , scrollTopIndex = newScrollTop
                                }

  notifyItemRemoveHandler w pos element

  when (oldSel /= newSelectedIndex) $
    notifySelectionHandler w

  return element

scrollBy :: Widget (ColumnList a) -> Int -> IO ()
scrollBy w amount = do
  height <- (fromIntegral . regionHeight) <$> getCurrentSize w
  updateWidgetState w (scrollBy' (height - 2) amount)
  notifySelectionHandler w

scrollBy' :: Int -> Int -> ColumnList a -> ColumnList a
scrollBy' height amount cl =
  cl { selectedIndex = new
     , scrollTopIndex = scroll }
  where len    = V.length $ listItems cl
        cur    = selectedIndex cl
        new    = max 0 (min (len - 1) (cur + amount))
        curScroll = scrollTopIndex cl
        scroll | new < curScroll = new
               | new >= curScroll + height = new - height + 1
               | otherwise = curScroll

scrollUp :: Widget (ColumnList a) -> IO ()
scrollUp w = scrollBy w (-1)

scrollDown :: Widget (ColumnList a) -> IO ()
scrollDown w = scrollBy w 1

getSelected :: Widget (ColumnList a) -> IO (Maybe (Int, a))
getSelected w = do
  list <- getState w
  case selectedIndex list of
    (-1) -> return Nothing
    i -> return $ Just (i, snd $ (listItems list) ! i)

pageUp :: Widget (ColumnList a) -> IO ()
pageUp w = do
  amt <- fromIntegral . regionHeight <$> getCurrentSize w
  scrollBy w (-1 * (amt - 2))

pageDown :: Widget (ColumnList a) -> IO ()
pageDown w = do
  amt <- fromIntegral . regionHeight <$> getCurrentSize w
  scrollBy w (amt - 2)

clearList :: Widget (ColumnList a) -> IO ()
clearList w = do
  updateWidgetState w $ \l ->
      l { selectedIndex = (-1)
        , scrollTopIndex = 0
        , listItems = V.empty
        }

setSelected :: Widget (ColumnList a) -> Int -> IO ()
setSelected wRef newPos = do
  list <- state <~ wRef
  case selectedIndex list of
    (-1) -> return ()
    curPos -> scrollBy wRef (newPos - curPos)

getListSize :: Widget (ColumnList a) -> IO Int
getListSize = ((V.length . listItems) <~~)

getListItem :: Widget (ColumnList a) -> Int -> IO (Maybe a)
getListItem wRef pos = do
  list <- state <~ wRef
  case pos >= 0 && pos < (V.length $ listItems list) of
    False ->  return Nothing
    True -> return $ Just $ snd (listItems list ! pos)

onSelectionChange :: Widget (ColumnList a)
                  -> (SelectionEvent a -> IO ())
                  -> IO ()
onSelectionChange = addHandler (selectionChangeHandlers <~~)

onItemAdded :: Widget (ColumnList a)
            -> (NewItemEvent a -> IO ())
            -> IO ()
onItemAdded = addHandler (itemAddHandlers <~~)

onItemRemoved :: Widget (ColumnList a)
              -> (RemoveItemEvent a -> IO ())
              -> IO ()
onItemRemoved = addHandler (itemRemoveHandlers <~~)

onItemActivated :: Widget (ColumnList a)
                -> (ActivateItemEvent a -> IO ())
                -> IO ()
onItemActivated = addHandler (itemActivateHandlers <~~)

activateCurrentItem :: Widget (ColumnList a) -> IO ()
activateCurrentItem wRef = do
  mSel <- getSelected wRef
  case mSel of
    Nothing -> return ()
    Just (pos, w) ->
      fireEvent wRef (itemActivateHandlers <~~) $ ActivateItemEvent pos w

notifySelectionHandler :: Widget (ColumnList a) -> IO ()
notifySelectionHandler wRef = do
  sel <- getSelected wRef
  case sel of
    Nothing ->
        fireEvent wRef (selectionChangeHandlers <~~) SelectionOff
    Just (pos, a) ->
        fireEvent wRef (selectionChangeHandlers <~~) $ SelectionOn pos a

notifyItemRemoveHandler :: Widget (ColumnList a) -> Int -> a -> IO ()
notifyItemRemoveHandler wRef pos k =
    fireEvent wRef (itemRemoveHandlers <~~) $ RemoveItemEvent pos k

notifyItemAddHandler :: Widget (ColumnList a) -> Int -> a -> IO ()
notifyItemAddHandler wRef pos k =
    fireEvent wRef (itemAddHandlers <~~) $ NewItemEvent pos k
