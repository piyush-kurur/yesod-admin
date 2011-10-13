{-|

This modules defines some type convenient type aliases

-}

module Yesod.Admin.Types
       ( AdminHandler
       , AdminId
       , AdminCRUD
       ) where

import Yesod
import Yesod.Admin.Crud
import Yesod.Admin.Subsite


-- | An alias for admin site handler.

type AdminHandler master v = GHandler (Admin master v) master

{-|

We assume that the master site is an instance of YesodPersist. This
is an alias for the database key to access element of type v.

-}

type AdminId master v = Key (YesodPersistBackend master) v

-- | An alias for CRUD related to admin sites.

type AdminCRUD master v  = CRUD (Admin master v) master v
