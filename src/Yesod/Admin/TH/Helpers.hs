{-|

Some helper functions that are useful for themplate Haskell code generation.

-}

module Yesod.Admin.TH.Helpers
       ( undefinedObjectOf
       , typeName
       , fieldName
       , entityColumn
       ) where

import Database.Persist
import Database.Persist.Base
import Yesod.Admin.Helpers

-- | Get an object associated with the given entity. The object
-- generated is undefined but usefull for type kludges.

undefinedObjectOf :: PersistEntity v
                  => EntityField v typ
                  -> v
undefinedObjectOf _ = undefined

-- | Get the type name associated with an object.
typeName :: PersistEntity v
         => v
         -> String
typeName = entityName . entityDef 

-- | Get the field name associated with an  EnitityField
fieldName :: PersistEntity v
          => EntityField v typ
          -> String
fieldName ef = fieldName' (undefinedObjectOf ef) ef

entityColumn :: PersistEntity v
             => EntityField v typ
             -> String
entityColumn = columnName . persistColumnDef 

fieldName' :: PersistEntity v
           => v
           -> EntityField v typ
           -> String
fieldName' v ef = camelCase $ unwords [ unCapitalise $ typeName v
                                      , entityColumn ef
                                      ]
