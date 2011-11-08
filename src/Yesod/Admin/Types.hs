{-|

This modules defines some type convenient type aliases

-}

module Yesod.Admin.Types
       ( AdminHandler
       , AdminId
       , AdminKVPair
       , AdminCRUD
       , AdminWidget
       ) where

import Yesod
import Yesod.Admin.Crud
import Yesod.Admin.Subsite


-- | An alias for admin site handler.

type AdminHandler master v = GHandler (Admin master v) master

-- | An alias for widgets of admin sites.

type AdminWidget master v = GWidget (Admin master v) master

{-|

We assume that the master site is an instance of YesodPersist. This
is an alias for the database key to access element of type v.

-}

type AdminId master v = Key (YesodPersistBackend master) v

{-|

This type captures the admin key value pairs. Key value pairs are
returned by many of the database functions. It is good to have this
type defined.

-}

type AdminKVPair master v = (AdminId master v, v)

-- | An alias for CRUD related to admin sites.

type AdminCRUD master v  = CRUD (Admin master v) master v
