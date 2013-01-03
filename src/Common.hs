-- | Functions that are useful in both UI variants
module Common
       ( formatDeviceLabel
       , deviceBoring
       ) where

import Data.List (intercalate)

import DBus.UDisks

-- | Get some form device label
-- This returns something suitable for the enduser not including the device file
-- (or Nothing, if there is none).
formatDeviceLabel :: Device -> Maybe String
formatDeviceLabel dev
  | title /= "" = Just title
  | vendor' /= "" || model' /= "" =
    Just (intercalate " " [vendor',model'])
  | otherwise = Nothing
  where title   = name dev
        vendor' = vendor dev
        model'  = model dev

-- | Whether a device is considered boring
-- I.e. a device that shouldn't be presented to the user or get a notification.
-- E.g. internal hard disks.
deviceBoring :: Device -> Bool
deviceBoring dev = or $ map ($dev) [internal, hasPartitions, not . hasMedia]
