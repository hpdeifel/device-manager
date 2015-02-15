{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DBus.DBusAbstraction
       ( DBusObject(..)
       , invoke
       , invoke'
       , getProperty
       , fromVariant'
       , matchSignal
       ) where

import Control.Applicative ((<$>))

import DBus
import DBus.Client

-- | Class for an object on the DBus
--
-- This is a little simplified, because DBus allows one object to have
-- more than one interface. If you need to use any other interface
-- than the one returned in getInterface, you have to call invoke'
-- instead of invoke.
--
-- The methods should be obvious, if you know some DBus
class DBusObject o where
  getObjectPath :: o -> ObjectPath
  getInterface  :: o -> InterfaceName
  getDestination :: o -> BusName

type ErrorMessage = String

-- Invoke a method of the object
invoke :: (DBusObject o)
          => Client             -- The DBus connection
          -> o                  -- The actual object
          -> MemberName         -- The name of the method to invoke
          -> [Variant]          -- The arguments, as Variants. See toVariant
          -> IO (Either ErrorMessage Variant)         -- The return value
invoke client obj member args = invoke' client obj (getInterface obj) member args

invoke' :: (DBusObject o) => Client -> o -> InterfaceName -> MemberName -> [Variant] -> IO (Either ErrorMessage Variant)
invoke' client obj interface member args = do
  res <- call client (methodCall (getObjectPath obj) interface member) {
    methodCallDestination = Just (getDestination obj),
    methodCallBody = args
    }
  return $ case res of
    Left err -> Left $ methodErrorMessage err
    Right res' -> Right (methodReturnBody res' !! 0)

getProperty :: (DBusObject o) => Client -> o -> String -> IO Variant
getProperty client obj property = do
  res <- invoke' client obj propertyInterface "Get" [toVariant (getInterface obj), toVariant property]
  case res of
    Left err -> error err
    Right res' -> return $ fromVariant' res'

fromVariant' :: (IsVariant a) => Variant -> a
fromVariant' v = case fromVariant v of
  Just val -> val
  Nothing  -> error ("Variant " ++ show v ++ " failed")

matchSignal :: (DBusObject o) => o -> MemberName -> MatchRule
matchSignal obj member = matchAny {
  matchPath        = Just (getObjectPath obj),
  matchInterface   = Just (getInterface obj),
--  matchDestination = Just (getDestination obj),
  matchMember      = Just member
}

propertyInterface :: InterfaceName
propertyInterface = "org.freedesktop.DBus.Properties"
