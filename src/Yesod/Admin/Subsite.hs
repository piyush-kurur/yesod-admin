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
       ( Crud
       , getCrud
       , Selection
       , getSelection
       -- * Routes
       -- $routes
       ) where

import Yesod

-- | The subsite that provides the crud interface.

data Crud master v = Crud

getCrud :: Crud master v
getCrud = Crud

{-

The roots are

/#ID        -- Read it
/create     -- Creat it
/update/#ID -- update it
/delete/#ID -- delete it

-}


instance ( YesodPersist master
         , PathPiece (Key (YesodPersistBackend master) v)
         ) => RenderRoute (Crud master v) where

         -- | The routes for crud operations.
         data Route (Crud master v)
                       = CreateR      -- create an object
                       | ReadR   (Key (YesodPersistBackend master) v)
                                      -- ^ view an object
                       | UpdateR (Key (YesodPersistBackend master) v)
                                      -- ^ update an objects
                       | DeleteR (Key (YesodPersistBackend master) v)
                                      -- ^ delete an object
                       deriving (Eq, Show, Read)

         renderRoute CreateR     = (["create"],[])
         renderRoute (ReadR   k) = ([toPathPiece k],[])
         renderRoute (UpdateR k) = (["update", toPathPiece k],[])
         renderRoute (DeleteR k) = (["delete", toPathPiece k],[])

-- | The subsite for selection objects.

data Selection master v = Selection
getSelection :: Selection master v
getSelection = Selection

{-

The routes are

/          -- The selection
/#Int      -- The th page
/action    -- The handler that performs the action

-}

instance RenderRoute (Selection master v) where
         -- | The routes for listing objects
         data Route (Selection master v) = ListR
                                         | ActionR
                                         | PageR Int deriving Eq


         renderRoute ListR     = ([],[])
         renderRoute ActionR   = (["action"],[])
         renderRoute (PageR p) = ([toPathPiece p],[])
