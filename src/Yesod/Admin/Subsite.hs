{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-|

This module defines the admin subsite data type. We distinguish
between two classes of admin subsites.

  1. Basic crud operations: This is supported as long as the persist
     backend of the master site is an instance of PersistStore. The
     datatype `Crud` captures this subsite.

  2. Selection: This is supported in addition to the basic crud
     operations if the persist backend of the master site is an
     instance of PersistQuery as well. The datatype Selection captures
     this subsite.

-}
module Yesod.Admin.Subsite
       ( Admin
       -- * Routes
       -- $routes
       ) where

import Yesod
import Data.Default

-- | The admin subsite

data Admin master v = Admin

-- | Get a default
instance Default (Admin master v) where
         def = Admin


{-

The roots are

/#ID            -- Read it
/create         -- Creat it
/update/#ID     -- update it
/delete/#ID     -- delete it
/selection      -- selection list
/selection/#Int -- Page
/action         -- Action page

-}

instance ( YesodPersist master
         , PathPiece (Key (YesodPersistBackend master) v)
         ) => RenderRoute (Admin master v) where

         -- | The routes for crud operations.
         data Route (Admin master v)
                       = CreateR      -- create an object
                       | ReadR   (Key (YesodPersistBackend master) v)
                                      -- ^ view an object
                       | UpdateR (Key (YesodPersistBackend master) v)
                                      -- ^ update an objects
                       | DeleteR (Key (YesodPersistBackend master) v)
                                      -- ^ delete an object
                       | ListR
                       | PageR Int
                       | ActionR
                       deriving (Eq, Show, Read)

         renderRoute CreateR     = (["create"],[])
         renderRoute (ReadR   k) = ([toPathPiece k],[])
         renderRoute (UpdateR k) = (["update", toPathPiece k],[])
         renderRoute (DeleteR k) = (["delete", toPathPiece k],[])
         renderRoute ListR       = (["list"],[])
         renderRoute (PageR pg)  = (["list", toPathPiece pg],[])
         renderRoute ActionR     = (["action"],[])
