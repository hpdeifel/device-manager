{-# LANGUAGE LambdaCase #-}
module Main where

import DBus.UDisks2
import Control.Monad

main :: IO ()
main = void $ withConnection $ \(con, objMap) -> do
  print objMap
  forever $ do
    nextEvent con >>= print
    putStrLn ""
