{-# LANGUAGE LambdaCase #-}
module Main where

import DBus.UDisks2
import Control.Monad
import Control.Concurrent.STM (atomically)

main :: IO ()
main = either print (const $ return ()) =<< withConnection (\(con, objMap) -> do
  print objMap
  forever $ do
    atomically (nextEvent con) >>= print
    putStrLn "")
