{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings         #-}
{-|

Sites with admin interfaces. 

-}
module Yesod.Admin.Class
       ( Administrable(..)
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

This class captures those master sites that have an admin interface to
values of type v. The master site should support
authentication. Therefore, YesodAdmin instance can be declared only
for master sites with authentication. Users might have differing
administrative rights. We capture this via the class function crudOf.
We might need to lookup the data base for administrative permissions
and hence the type of crudOf is a GHandler.

-}

class YesodAuth master => YesodAdmin master v where
