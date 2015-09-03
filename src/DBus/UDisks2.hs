module DBus.UDisks2
       ( Connection
       , connect
       , disconnect
       , withConnection
       , Event(..)
       , nextEvent
       ) where

import DBus.UDisks2.Internal
