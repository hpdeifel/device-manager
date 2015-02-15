module ErrorLogger where

import Data.Text

class ErrorLogger a where
  logError :: a -> Text -> IO ()
