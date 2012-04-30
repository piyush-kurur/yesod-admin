{-|

This modules defines some type convenient type aliases

-}

module Yesod.Admin.Types
       ( Admin
       , getAdmin
       , Crud(..)
       , Selection(..)
       , SiteKey
       , SiteKVPair
       , DBAction(..)
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

-- | The admin subsite. This is the site that has the crud and
-- selection subsites of all the entities of your application. You can
-- hook this site as a subsite to your main site. However, the routes
-- and dispatch instance for this site can only be declared at the
-- time of use as the entities that we want to have an admin subsites
-- for is known only at that time.
data Admin master = Admin

-- | Function used to hook the admin subsite into the main site.
getAdmin :: master -> Admin master
getAdmin = const Admin

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

data DBAction b m v = DBDelete            -- ^ A delete action
                    | DBUpdate (Update v) -- ^ An update action
                    | DBCustom { runCustomAction :: Key b v -> b m () }
                                            -- ^ A custom action

-- | A type alias for widgets of the admin subsite.
type CrudWidget  master v  = GWidget  (Crud master v) master

-- | A type alias for handlers of the admin subsite.
type CrudHandler master v  = GHandler (Crud master v) master


-- | A type alias for widgets of the selection subsite.
type SelectionWidget  master v  = GWidget  (Selection master v) master

-- | A type alias for handlers of the selection subsite.
type SelectionHandler master v  = GHandler (Selection master v) master
