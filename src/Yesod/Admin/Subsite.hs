{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-|

This defines the admin subsite data type. 

-}
module Yesod.Admin.Subsite
       ( Admin
       -- * Routes
       -- $routes
       , AdminRoute(..)
       , getAdmin
       ) where

import Yesod

-- | The foundation type for admin subsite of type v on the site
-- master.

data Admin master v = Admin

-- | Get a default instance here.

getAdmin :: Admin master v
getAdmin = Admin

{-

The route that we want for an admin interfaces is the following

/         list all objects of type v
/page/#Int                 Show the nth page.
/create                    C - creation
/read/#(Key b v)           R - read the associate object
/update/#(Key b v)         U - Update the associate object
/delete/#(Key b v)         D - Delete the associate object

Unfortunately the current subsite generation TH code does not support
these generic routes hence we have to code these by hand.

-}

{- $routes

In case you want to link to various admin pages you will need to use
the constructors of this type. Make sure to lift the routes to roots
of the master site. See the function
@"Yesod.Admin.Handlers".'toMasterRoute'@

-}

-- | The data type that captures the admin related routes of an
-- object.

data AdminRoute backend v = AdminListR          -- ^ Route to list the objects.
                          | AdminPageR   Int    -- ^ Route to paged listing.
                          | AdminCreateR        -- ^ Route to create an object
                          | AdminReadR   (Key backend v)
                                                -- ^ Route to view an object
                          | AdminUpdateR (Key backend v)
                                                -- ^ Route to update an objects
                          | AdminDeleteR (Key backend v)
                                                -- ^ Route to delete an object
                          deriving (Eq, Show, Read)

instance SinglePiece (Key backend v)
         => RenderRoute (AdminRoute backend v) where
         renderRoute AdminListR       = ([],[])
         renderRoute (AdminPageR  p)  = (["page", toSinglePiece p], [])
         renderRoute AdminCreateR     = (["create"],[])
         renderRoute (AdminReadR   k) = (["read",   toSinglePiece k],[])
         renderRoute (AdminUpdateR k) = (["update", toSinglePiece k],[])
         renderRoute (AdminDeleteR k) = (["delete", toSinglePiece k],[])

type instance Route (Admin master v) = AdminRoute (YesodPersistBackend master) v
