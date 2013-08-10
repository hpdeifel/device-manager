{-# LANGUAGE DeriveDataTypeable, MultiWayIf #-}

-- | This widget is based on the List widget, but adds the ability for columns.
module Graphics.Vty.Widgets.ColumnList
       ( ColumnList()
       , ColumnSpec(ColumnSpec)
       , ColumnSize(..)

       -- ** List creatoin
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

       , clearList
       , setSelected

       -- ** List inspection
       , getListSize
       , getSelected
       , getListItem
       ) where

import Graphics.Vty.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.DisplayRegion
import Graphics.Vty.Image
import Graphics.Vty.Widgets.Table (ColumnSize(..))
import Graphics.Vty.Widgets.Text
import Graphics.Vty.Widgets.Borders
import Graphics.Vty.Widgets.Fixed
import Graphics.Vty.LLInput

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Text (Text)
import Data.List
import Data.Typeable
import Data.Word (Word)

import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception

import Debug.Trace

data ListError = BadItemIndex Int
               -- ^The specified position could not be used to remove
               -- an item from the list.
               | ResizeError
               | BadListWidgetSizePolicy
               -- ^The type of widgets added to the list grow
               -- vertically, which is not permitted.
                 deriving (Show, Typeable)

instance Exception ListError

data ColumnList a = ColumnList {
  listItems      :: V.Vector ([Widget FormattedText], a),
  columns        :: [ColumnSpec a],
  selectedAttr   :: Attr, -- TODO differentiate between focused and unfocused
  selectedIndex  :: Int,
  scrollTopIndex :: Int,
  headerWidgets  :: [Widget FormattedText],
  borderWidget   :: Widget HBorder
}

instance Show (ColumnList a) where
  show _ = "ColumnList"

data ColumnSpec a = ColumnSpec {
  title    :: Text,
  showFunc :: a -> Text
}

instance Show (ColumnSpec a) where
  show cs = concat [ "ColumnSpec { "
                   , "title = ", show $ title cs
                   ]

newListData :: Attr -> [ColumnSpec a] -> IO (ColumnList a)
newListData selAttr cols = do
  header <- mapM (plainText . title) cols
  border <- hBorder

  return $ ColumnList { listItems = V.empty
                      , columns = cols
                      , selectedAttr = selAttr
                      , selectedIndex = (-1)
                      , scrollTopIndex = 0
                      , headerWidgets = header
                      , borderWidget = border
                      }

renderList :: Widget (ColumnList a) -> DisplayRegion -> RenderContext -> IO Image
renderList w region context = do
  cl <- getState w
  h <- renderLine (region_width region) context (headerWidgets cl)
  b <- render (borderWidget cl) region context
  height <- (fromIntegral . region_height) <$> getCurrentSize w
  let curScroll = scrollTopIndex cl
      items     = V.length (listItems cl) - 1
      visible = [curScroll..(min (curScroll + height) items)]
  body <- forM visible $ \i -> 
    let line = fst (listItems cl ! i)
    in if (i == selectedIndex cl)
       then renderLine (region_width region) context { overrideAttr = selectedAttr cl} line
       else renderLine (region_width region) context line

  let size = (region_width region, region_height region)
      remaining = fromIntegral $ max 0 (fromIntegral (region_height region) - 2 - length visible)
      filler = if remaining == 0 then empty_image
               else char_fill (normalAttr context) ' ' (region_width region) remaining

  return $ crop size $
    (h <-> b <-> foldr (<->) empty_image body <-> filler)

renderLine :: Word -> RenderContext -> [Widget FormattedText] -> IO Image
renderLine width' context cols = do
  imgs <- mapM rend $ zip widths cols
  return $ horiz_cat $ intersperse (background_fill 2 1) imgs

  where items = length cols
        width = fromIntegral width' - space
        space = (items - 1) * 2
        minLengths = replicate items (width `div` items)
        ones = replicate (width `mod` items) 1
        widths = zipWith (+) minLengths (ones ++ repeat 0)
        mkRegion w = DisplayRegion (fromIntegral w) 1
        rend (width, widget) = do
          fix <- hFixed width widget
          render fix (mkRegion width) context
        
               
newList :: Attr -> [ColumnSpec a] -> IO (Widget (ColumnList a))
newList selAttr cols = do
  list <- newListData selAttr cols
  wRef <- newWidget list $ \w ->
    w { render_ = renderList
      , growHorizontal_ = const $ return True
      , growVertical_ = const $ return False
      , getCursorPosition_ = const $ return Nothing
      , keyEventHandler = listKeyEvent
      }
                         
  return wRef

listKeyEvent :: Widget (ColumnList a) -> Key -> [Modifier] -> IO Bool
listKeyEvent w KUp _ = scrollUp w >> return True
listKeyEvent w KDown _ = scrollDown w >> return True
listKeyEvent w KPageUp _ = pageUp w >> return True
listKeyEvent w KPageDown _ = pageDown w >> return True
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
  height <- (fromIntegral . region_height) <$> getCurrentSize w

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

  where mkText e (ColumnSpec _ f) = plainText (f e)
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

  return element

scrollBy :: Widget (ColumnList a) -> Int -> IO ()
scrollBy w amount = do
  height <- (fromIntegral . region_height) <$> getCurrentSize w
  updateWidgetState w (scrollBy' (height - 2) amount)

scrollBy' :: Int -> Int -> ColumnList a -> ColumnList a
scrollBy' height amount cl = traceShow (scroll,height,new) $
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
  amt <- fromIntegral . region_height <$> getCurrentSize w
  scrollBy w (-1 * (amt - 2))

pageDown :: Widget (ColumnList a) -> IO ()
pageDown w = do
  amt <- fromIntegral . region_height <$> getCurrentSize w
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
