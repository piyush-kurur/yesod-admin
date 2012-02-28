{-# LANGUAGE TemplateHaskell    #-}
{-|

Some helper functions that are useful for themplate Haskell code generation.

-}

module Yesod.Admin.TH.Helpers
       ( undefinedObjectOf
       , typeName
       , fieldName

       , mkInstance
       , persistType
       , persistStoreP
       , monadP
       , entityAdmin
       , getEntityAdmin
       , textL
       ) where

import Language.Haskell.TH
import Data.Text( Text, unpack)
import Database.Persist.EntityDef
import Yesod
import Yesod.Admin.Helpers

entityName :: PersistEntity v => v -> String
entityName = unpack . unHaskellName . entityHaskell . entityDef

-- | Get an object associated with the given entity. The object
-- generated is undefined but useful for type kludges.

undefinedObjectOf :: PersistEntity v
                  => EntityField v typ
                  -> v
undefinedObjectOf _ = undefined

-- | Get the type name associated with an object.
typeName :: PersistEntity v
         => v
         -> String
typeName = entityName

-- | Get the field name associated with an  EnitityField
fieldName :: PersistEntity v
          => v
          -> String
          -> String
fieldName v fname = camelCase $ unwords [ unCapitalise $ typeName v
                                        , fname
                                        ]
{-
entityColumn :: PersistEntity v
             => EntityField v typ
             -> String
entityColumn = columnName . persistColumnDef
-}

-- | Create an instance declaration.
mkInstance :: [PredQ] -> Name -> [TypeQ] -> [DecQ] -> DecQ
mkInstance context cls args defs = instanceD (cxt context) inst defs
     where inst = foldl appT (conT cls) args


-- ^ Generates PersistStore constraint.
persistStoreP :: TypeQ -> TypeQ -> PredQ
persistStoreP b m = classP ''PersistStore [b,m]

-- ^ Generate Monad constraint.
monadP :: TypeQ -> PredQ
monadP m = classP ''Monad [m]

-- | The type name of a persist entity.
persistType :: PersistEntity v
            => v
            -> TypeQ
persistType v = conT . mkName $ entityName v ++ "Generic"

-- | Generates the entities type alias name

entityAdmin :: String -- ^ the persistent entity
            -> String
entityAdmin entity = entity ++ "Admin"

getEntityAdmin :: String  -- ^ the persistent entity
               -> String
getEntityAdmin entity = "get" ++ entity ++ "Admin"

textL t = sigE strL $ conT ''Text
     where strL = litE $ stringL (unpack t)
