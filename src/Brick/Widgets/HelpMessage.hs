{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.HelpMessage
       ( Title
       , KeyBindings
       , help
       , helpAttr
       ) where

import Brick
import Brick.Markup
import Brick.Widgets.Border
import Graphics.Vty
import Data.Text.Markup
import Data.Text (Text)
import Data.Monoid
import Data.List
import Control.Lens

type Title = Text

-- [(Title, [(Key, Description)])]
type KeyBindings = [(Title, [(Text, Text)])]

help :: KeyBindings -> Widget
help bindings = center $ helpWidget bindings

center :: Widget -> Widget
center w = Widget Fixed Fixed $ do
  c <- getContext
  res <- render w
  let rWidth = res^.imageL.to imageWidth
      rHeight = res^.imageL.to imageHeight
      x = (c^.availWidthL `div` 2) - (rWidth `div` 2)
      y = (c^.availHeightL `div` 2) - (rHeight `div` 2)

  render $ translateBy (Location (x,y)) $ raw (res^.imageL)

helpWidget :: KeyBindings -> Widget
helpWidget bindings = Widget Fixed Fixed $ do
  c <- getContext

  render $ hLimit (min 80 $ c^.availWidthL `div` 2) $
    borderWithLabel (txt "Help") $
    vBox $ intersperse (txt " ") $
    map (uncurry section) bindings

key :: Text -> Text -> Widget
key k h =  markup (("  " <> k) @? (helpAttr <> "key"))
       <+> padLeft Max (markup (h @? (helpAttr <> "description")))

helpAttr :: AttrName
helpAttr = "help"

section :: Title -> [(Text, Text)] -> Widget
section title keys =  markup ((title <> ":") @? (helpAttr <> "title"))
                  <=> vBox (map (uncurry key) keys)
