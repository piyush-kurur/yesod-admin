{-# LANGUAGE TemplateHaskell    #-}
{-|

Some helper functions that are useful for themplate Haskell code generation.

-}

module Yesod.Admin.TH.Helpers
       ( undefinedObjectOf
       , typeName
       , fieldName
       , entityColumn
       , mkInstance
       , persistType
       , persistBackendP
       , monadP
       , entityAdmin
       , getEntityAdmin
       ) where

import Database.Persist
import Database.Persist.Base
import Yesod.Admin.Helpers
import Language.Haskell.TH

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
          => v
          -> String
          -> String
fieldName v fname = camelCase $ unwords [ unCapitalise $ typeName v
                                        , fname
                                        ]

entityColumn :: PersistEntity v
             => EntityField v typ
             -> String
entityColumn = columnName . persistColumnDef 


-- | Create an instance declaration.
mkInstance :: [PredQ] -> Name -> [TypeQ] -> [DecQ] -> DecQ
mkInstance context cls args defs = instanceD (cxt context) inst defs
     where inst = foldl appT (conT cls) args


-- ^ Generates PersistBackend constraint.
persistBackendP b m = classP ''PersistBackend [b,m]
-- ^ Generate Monad constraint.
monadP m = classP ''Monad [m]

-- | The type name of a persist entity.
persistType :: PersistEntity v
            => v
            -> TypeQ
persistType = conT . mkName . entityName . entityDef
      
-- | Generates the entities type alias name
entityAdmin :: String -- ^ the persistent entity
            -> String
entityAdmin entity = entity ++ "Admin"

getEntityAdmin :: String  -- ^ the persistent entity
               -> String
getEntityAdmin entity = "get" ++ entity ++ "Admin"
