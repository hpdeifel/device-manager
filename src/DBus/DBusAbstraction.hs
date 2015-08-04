{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module DBus.DBusAbstraction
       ( DBusObject(..)
       , DBusInterface(..)
       , Implements
       , SaneDBusObject
       , WithIface()
       , using
       , invoke
       , getProperty
       , getAllProperties
       , getInterfaces
       , fromVariant'
       , matchSignal
       , introspect
       , Introspectable(..)
       , Properties(..)
       , JustAPath(..)
       ) where

import DBus
import DBus.Client
import DBus.Introspection

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import Control.Exception

class DBusInterface i where
  getInterface :: i -> InterfaceName

class DBusObject o where
  type DefaultInterface o :: *
  getObjectPath :: o -> ObjectPath
  getDestination :: o -> BusName

class (DBusObject o, DBusInterface i) => Implements o i

data WithIface o i = WithIface o

using :: o -> i -> WithIface o i
using o _ = WithIface o

ifaceOf :: DBusObject o => o -> DefaultInterface o
ifaceOf = const undefined

instance DBusObject o => DBusObject (WithIface o i) where
  type DefaultInterface (WithIface o i) = i
  getObjectPath (WithIface o) = getObjectPath o
  getDestination (WithIface o) = getDestination o

instance Implements o i => Implements (WithIface o i) i

type SaneDBusObject o = (DBusObject o, Implements o (DefaultInterface o))

-- Invoke a method of the object
invoke :: (SaneDBusObject o)
          => Client             -- ^ The DBus connection
          -> o                  -- ^ The actual object
          -> MemberName         -- ^ The name of the method to invoke
          -> [Variant]          -- ^ The arguments, as Variants. See toVariant
          -> IO (Either MethodError Variant)         -- The return value
invoke client obj = invoke' client obj (ifaceOf obj)

invoke' :: (Implements o i) => Client -> o -> i -> MemberName -> [Variant] -> IO (Either MethodError Variant)
invoke' client obj iface member args = do
  res <- call client (methodCall (getObjectPath obj) (getInterface iface) member) {
    methodCallDestination = Just (getDestination obj),
    methodCallBody = args
    }
  return $ (head . methodReturnBody) <$> res

getProperty :: (SaneDBusObject o, Implements o Properties) => Client -> o -> String -> IO Variant
getProperty client obj prop = do
  res <- invoke client (obj `using` Properties) "Get" [toVariant (getInterface (ifaceOf obj)), toVariant prop]
  case res of
    Left err -> throw (clientError $ show err)
    Right res' -> return $ fromVariant' res'

getAllProperties :: (Implements o Properties, DBusInterface i)
                 => Client -> o -> i -> IO (Map String Variant)
getAllProperties client obj iface =
  invoke client (obj `using` Properties) "GetAll" [toVariant (getInterface iface)]
    >>= either (throw.clientError.show) (return.fromVariant')

getInterfaces :: (DBusObject o, Implements o Introspectable)
              => Client -> o -> IO [DBus.InterfaceName]
getInterfaces client obj = do
  res <- introspect client obj
  return $ map interfaceName $ objectInterfaces res

fromVariant' :: (IsVariant a) => Variant -> a
fromVariant' v = fromMaybe (error ("Variant " ++ show v ++ " failed")) $ fromVariant v

matchSignal :: (SaneDBusObject o) => o -> MemberName -> MatchRule
matchSignal obj member = matchAny {
  matchPath        = Just (getObjectPath obj),
  matchInterface   = Just (getInterface (ifaceOf obj)),
--  matchDestination = Just (getDestination obj),
  matchMember      = Just member
}

data Properties = Properties
instance DBusInterface Properties where
  getInterface _ = "org.freedesktop.DBus.Properties"

data Introspectable = Introspectable
instance DBusInterface Introspectable where
  getInterface _ = "org.freedesktop.DBus.Introspectable"

introspect :: (Implements o Introspectable) => Client -> o -> IO Object
introspect client obj = invoke client (obj `using` Introspectable) "Introspect" []
                        >>= either (throw.clientError.show) (return.parseXML (getObjectPath obj).fromVariant')
                        >>= maybe (error "Could not parse DBus XML") return

data JustAPath = JustAPath BusName ObjectPath

instance DBusObject JustAPath where
  type DefaultInterface JustAPath = Introspectable
  getObjectPath (JustAPath _ path) = path
  getDestination (JustAPath name _) = name

instance Implements JustAPath Introspectable

instance Implements JustAPath Properties

instance DBusInterface InterfaceName where
  getInterface = id
