module DBus.UDisks2.Operations where

import DBus.UDisks2.Types

-- Contains available operations (dbus methods) for every supported interface.

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

-- The following methods are not implemented, but should be

-- SetLabel (IN  s     label,
--           IN  a{sv} options);
-- Mount    (IN  a{sv} options,
--           OUT s     mount_path);
-- Unmount  (IN  a{sv} options);


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
