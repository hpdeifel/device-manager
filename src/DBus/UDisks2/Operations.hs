{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase, FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module DBus.UDisks2.Operations
       ( Operation
       , runOperation
       , fsMount
       , fsUnmount
       ) where

import DBus.UDisks2.Types
import DBus.UDisks2.Internal

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Data.Proxy

import qualified DBus
import qualified DBus.DBusAbstraction as DBus
import DBus.DBusAbstraction (DBusInterface(..),DBusObject(..), Implements)

-- Contains available operations (dbus methods) for every supported interface.

type Operation = ExceptT Text (ReaderT Connection IO)

runOperation :: Connection -> Operation a -> IO (Either Text a)
runOperation con = flip runReaderT con . runExceptT

----------------------------------------------------------------------
-- Block -------------------------------------------------------------
----------------------------------------------------------------------

-- The following methods are not implemented, but should be:

-- AddConfigurationItem    (IN  (sa{sv})  item, IN  a{sv}     options);
-- RemoveConfigurationItem (IN  (sa{sv})  item,
--                          IN  a{sv}     options);
-- UpdateConfigurationItem (IN  (sa{sv})  old_item,
--                          IN  (sa{sv})  new_item,
--                          IN  a{sv}     options);
-- GetSecretConfiguration  (IN  a{sv}     options,
--                          OUT a(sa{sv}) configuration);
-- Format                  (IN  s         type,
--                          IN  a{sv}     options);
-- Rescan                  (IN  a{sv}     options);

-- The following methods can't be implemented, because the dbus library
-- currently can't deal with filedescriptors.

-- OpenForBackup           (IN  a{sv}     options,
--                          OUT h         fd);
-- OpenForRestore          (IN  a{sv}     options,
--                          OUT h         fd);
-- OpenForBenchmark        (IN  a{sv}     options,
--                          OUT h         fd);


----------------------------------------------------------------------
-- FileSystem --------------------------------------------------------
----------------------------------------------------------------------

instance DBusObject FileSystemIface where
  type DefaultInterface FileSystemIface = FileSystemIface
  getObjectPath = view (fsObj.asPath)
  getDestination _ = udisksDestination

instance Implements FileSystemIface FileSystemIface

-- The following methods are not implemented, but should be

-- SetLabel (IN  s     label,
--           IN  a{sv} options);

type MountOptions = Map Text DBus.Variant

fsMount :: FileSystemIface -> MountOptions -> Operation Text
fsMount iface opts = invoke iface "Mount" [DBus.toVariant opts]

-- Unmount  (IN  a{sv} options);
fsUnmount :: FileSystemIface -> MountOptions -> Operation ()
fsUnmount iface opts = invoke iface "Unmount" [DBus.toVariant opts]


----------------------------------------------------------------------
-- Partitition -------------------------------------------------------
----------------------------------------------------------------------

-- The following methods are not implemented, but should be

-- SetType  (IN  s     type,
--           IN  a{sv} options);
-- SetName  (IN  s     name,
--           IN  a{sv} options);
-- SetFlags (IN  t     flags,
--           IN  a{sv} options);
-- Delete   (IN  a{sv} options);

----------------------------------------------------------------------
-- Loop --------------------------------------------------------------
----------------------------------------------------------------------

-- The following methods are not implemented, but should be

-- Delete       (IN  a{sv} options);
-- SetAutoclear (IN  b     value,
--               IN  a{sv} options);


----------------------------------------------------------------------
-- Drive -------------------------------------------------------------
----------------------------------------------------------------------

-- The following methods are not implemented, but should be

-- Eject            (IN  a{sv} options);
-- SetConfiguration (IN  a{sv} value,
--                   IN  a{sv} options);
-- PowerOff         (IN  a{sv} options);


----------------------------------------------------------------------
-- Ata ---------------------------------------------------------------
----------------------------------------------------------------------

-- The following methods are not implemented, but should be

-- SmartUpdate        (IN  a{sv}            options);
-- SmartGetAttributes (IN  a{sv}            options,
--                     OUT a(ysqiiixia{sv}) attributes);
-- SmartSelftestStart (IN  s                type,
--                     IN  a{sv}            options);
-- SmartSelftestAbort (IN  a{sv}            options);
-- SmartSetEnabled    (IN  b                value,
--                     IN  a{sv}            options);
-- PmGetState         (IN  a{sv}            options,
--                     OUT y                state);
-- PmStandby          (IN  a{sv}            options);
-- PmWakeup           (IN  a{sv}            options);
-- SecurityEraseUnit  (IN  a{sv}            options);


-- Helpers
asPath :: Iso' ObjectId DBus.ObjectPath
asPath = iso getPath ObjectId
  where getPath (ObjectId path) = path

instance FillIface i => DBusInterface i where
  getInterface _ = DBus.interfaceName_ $ ifaceName (Proxy :: Proxy i)

udisksDestination :: DBus.BusName
udisksDestination = "org.freedesktop.UDisks2"

invoke :: (DBus.SaneDBusObject o, DBus.IsVariant a)
       => o -> DBus.MemberName -> [DBus.Variant] -> Operation a
invoke obj member args = do
  client <- asks conClient
  liftIO (DBus.invoke client obj member args) >>= \case
    Left err -> throwError $ T.pack $ show err
    Right res -> return $ DBus.fromVariant' res

-- NOTE: This is a hack to allow methods without return values to work. It
-- crucially depends on the fact that the argument for fromVariant is not
-- evaluated.
instance DBus.IsVariant () where
  toVariant = undefined -- Ugly as hell
  fromVariant _ = Just ()
