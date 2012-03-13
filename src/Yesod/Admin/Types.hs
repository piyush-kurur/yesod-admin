{-|

This modules defines some type convenient type aliases

-}

module Yesod.Admin.Types
       ( SiteKey
       , SiteKVPair
       , Action
       ) where

import Yesod
import Database.Persist.Query.Internal

{-|

We assume that the master site is an instance of YesodPersist. This is
an alias for the database key to access element of type v.

-}

type SiteKey master v = Key (YesodPersistBackend master) v

{-|

This type captures the admin key value pairs. Key value pairs are
returned by many of the database functions. It is good to have this
type defined.

-}

type SiteKVPair master v = (SiteKey master v, v)

-- | This types captures actions that can be applied from selection
-- page.

data Action b m v = ActionDelete            -- ^ A delete action
                  | ActionUpdate (Update v) -- ^ An update action
                  | ActionCustom { runCustomAction :: Key b v -> b m () }
                                            -- ^ A custom action
