{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-|

Sites with admin interfaces. 

-}
module Yesod.Admin.Class
       ( Administrable(..)
       , InlineDisplay(..)
       , ColumnDisplay(..)
       , YesodAdmin(..)
       ) where

import Data.Text(Text, append, pack)
import Yesod.Auth
import Database.Persist.Base
import Yesod.Admin.Helpers
import Yesod.Admin.Subsite
import Yesod.Admin.Types

-- | This class captures objects that have an admin
-- interfaces. Minimum complete definition includes the data type
-- @'Column' v@ and the function 'columnTitle'.

class PersistEntity v => Administrable v where

      -- | The name of the object. Used in various places for example
      -- in titles of admin pages. The default values is the entity
      -- of the given persistent type.
      objectSingular :: v -> Text
      objectSingular = pack . unCamelCase . entityName . entityDef

      -- | The plural form of the object. By default an `s' is
      -- appended to the singular form.
      objectPlural  :: v -> Text
      objectPlural  v = objectSingular v `append` "s"

      -- | Abstract columns of the type. These columns can appear in
      -- listings and access control.  Besides the usual database
      -- columns they may capture other constructed columns.
      data Column v     :: *

      -- | The title of the given column.
      columnTitle :: Column v -> Text
      
      -- | In listing of the object what all columns should be listed.
      -- This can be empty in which case the inline display of the
      -- object is used.
      listColumns :: [Column v]
      listColumns = []
      
      -- | Controls in which order the objects are listed. By default
      -- no sorting is done, i.e. it is governed by the database
      -- backend.
      listSort   :: [SelectOpt v]
      listSort = []

      -- | How many elements to be shown on a page. The default value
      -- is 20. 
      objectsPerPage :: v -> Int
      objectsPerPage = const 20

{-|

Admin sites need to show objects in inline text or in listings of
entities in an human readable ways. This class captures objects that
can be shown inline on admin sites. The class might look unnecessarily
complicates; after all why not declare the member function
`inlineDisplay` to just return a Text instead of a monadic action. The
answer lies in the fact that for some objects like database id's, it
makes more sense to display the inline text of the objects that the id
points to rather than the plain id itself. This would require hitting
the database which means it cannot be a pure Haskell function.

-}

class PersistBackend b m => InlineDisplay b m a where
      inlineDisplay :: a -> b m Text

instance PersistBackend b m => InlineDisplay b m Text where
         inlineDisplay = return

instance PersistBackend b m => InlineDisplay b m String where
         inlineDisplay = return . pack

instance (Show a, PersistBackend b m) => InlineDisplay b m a where
         inlineDisplay = return . pack . show

instance ( PersistEntity v
         , PersistBackend b m
         , InlineDisplay b m v
         ) => InlineDisplay b m (Key b v) where
        
         inlineDisplay key = do maybev <- get key
                                maybe (return "Bad Key") inlineDisplay maybev

-- | This class captures display of columns of an object. Like in the
-- case of inline display, displaying certain columns of v require
-- hitting the database and hence the function is not a pure haskell
-- function.

class (PersistBackend b m, Administrable v) => ColumnDisplay b m v where
      columnDisplay     :: Column v  -- ^ The column
                        -> v         -- ^ The value whose column is required
                        -> b m Text

{-|

This class captures those master sites that have an admin interface to
values of type v. The master site should support
authentication. Therefore, YesodAdmin instance can be declared only
for master sites with authentication. Users might have differing
administrative rights. We capture this via the class function crudOf.
We might need to lookup the data base for administrative permissions
and hence the type of crudOf is a GHandler.

-}

class YesodAuth master => YesodAdmin master v where
