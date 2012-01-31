{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
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
       , getAdmin
       ) where

import Yesod

-- | The foundation type for admin subsite of type v on the site
-- master.

data Admin master v = Admin

-- | Get a default instance here.

getAdmin :: Admin master v
getAdmin = Admin

{- Developer's notes.

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

For any site/subsite @a@, the routes of the site is captured by the
associated data type @'Route' a@. The constructors of this associated
type is what you use in your templates. 
There are two ways to perform the lift. Consider the first situation
where you want to link to the admin pages of the Person entity.
Let the admin route be as follows.

> /admin/person/ PersonAdminR PersonAdmin getPersonAdmin

where @PersonAdmin@ is a type alias @'Admin' Site Person@ and
getPersonAdmin is the specialised form of @`getAdmin`@. Then the
constructor @PersonAdminR@ can be used to lift the
route. I.e. @PersonAdminR `AdminListR`@ is the route to the listing of
@Persons@, @PersonAdminR $ `AdminReadR` id@ is the route to view the
person with database id @id@ etc.  


In certain cases the route where the admin is hooked is not known.
For example all the code in this library does not know how the
individual admin sites are hooked. Use the combinator
@"Yesod.Admin.Handlers".'toMasterRoute'@ in such a case.

-}

-- | The routes in the admin site.

instance ( YesodPersist master
         , PathPiece (Key (YesodPersistBackend master) v)
         ) 
         => RenderRoute (Admin master v) where
         data Route (Admin master v)
                       = AdminListR        --
                       | AdminPageR  Int   -- the nth page of listing
                       | AdminCreateR      -- create an object
                       | AdminReadR   (Key (YesodPersistBackend master) v)
                                           -- ^ view an object
                       | AdminUpdateR (Key (YesodPersistBackend master) v)
                                           -- ^ update an objects
                       | AdminDeleteR (Key (YesodPersistBackend master) v)
                                           -- ^ delete an object
                       deriving (Eq, Show, Read)
         
         renderRoute AdminListR       = ([],[])
         renderRoute (AdminPageR  p)  = (["page", toPathPiece p], [])
         renderRoute AdminCreateR     = (["create"],[])
         renderRoute (AdminReadR   k) = (["read",   toPathPiece k],[])
         renderRoute (AdminUpdateR k) = (["update", toPathPiece k],[])
         renderRoute (AdminDeleteR k) = (["delete", toPathPiece k],[])
         

