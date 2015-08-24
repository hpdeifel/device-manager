{-# LANGUAGE LambdaCase #-}
module Main where

import DBus.UDisks2

import Control.Monad
import Control.Concurrent.STM

main :: IO ()
main = void $ withConnection $ \(_, objMap, events) -> do
  print objMap
  forever $
    atomically (readTQueue events) >>= print
