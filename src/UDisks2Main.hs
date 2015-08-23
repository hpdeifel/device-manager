{-# LANGUAGE LambdaCase #-}
module Main where

import DBus.UDisks2

import Control.Monad.Trans.Except
import qualified Data.Text.IO as T
import Control.Concurrent

main :: IO ()
main = withConnection $ \con -> do
  runExceptT (getInitialObjects con) >>= \case
    Left e -> putStr "Error: " >> T.putStrLn e
    Right m -> putStrLn "Success!" >> print m
  connectSignals con
  threadDelay (60 * 1000 * 1000)
