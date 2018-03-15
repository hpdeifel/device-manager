-- | Same as 'txt', but stupidly wraps long lines
module Brick.Widgets.WrappedText
       ( wrappedText
       ) where

import           Brick
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as T

wrappedText :: Text -> Widget n
wrappedText theText = Widget Fixed Fixed $ do
  ctx <- getContext
  let newText = wrapLines (ctx^.availWidthL) theText
  render $ txt newText


wrapLines :: Int -> Text -> Text
wrapLines width = T.unlines . concat . map wrap . T.lines
  where wrap = T.chunksOf width
