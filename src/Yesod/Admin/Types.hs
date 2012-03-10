{-|

This modules defines some type convenient type aliases

-}

module Yesod.Admin.Types
       ( SiteKey
       , SiteKVPair
       ) where

import Yesod

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
