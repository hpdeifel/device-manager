{-# LANGUAGE LambdaCase #-}
module Main where

import DBus.UDisks2.Simple
import Control.Monad

main :: IO ()
main = either print (const $ return ()) =<< withConnection (\(con, objects) -> do
  print objects
  forever $ do
    nextEvent con >>= print
    putStrLn "")
