{-|

This modules defines some type convenient type aliases

-}

module Yesod.Admin.Types
       ( Crud
       , Selection
       , SiteKey
       , SiteKVPair
       , Action
       , CrudHandler
       , CrudWidget
       , SelectionHandler
       , SelectionWidget
       ) where

import Yesod
import Database.Persist.Query.Internal
import Data.Default

-- | The crud subsite
data Crud master v = Crud

-- | The selection subsite
data Selection master v = Selection

-- | Default value for the Crud site
instance Default (Crud master v) where
         def = Crud

-- | Default value of Selection site
instance Default (Selection master v) where
         def = Selection



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

-- | A type alias for widgets of the admin subsite.
type CrudWidget  master v  = GWidget  (Crud master v) master

-- | A type alias for handlers of the admin subsite.
type CrudHandler master v  = GHandler (Crud master v) master


-- | A type alias for widgets of the selection subsite.
type SelectionWidget  master v  = GWidget  (Selection master v) master

-- | A type alias for handlers of the selection subsite.
type SelectionHandler master v  = GHandler (Selection master v) master
