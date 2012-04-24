{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-|

Some helper functions that are useful for themplate Haskell code generation.

-}

module Yesod.Admin.TH.Helpers
       ( undefinedObjectOf
       , mkInstance
       , persistType
       , persistStoreP
       , monadP
       , entityAdmin
       , getEntityAdmin
       , textL
       , mkNameT
       , singleArgFunc
       ) where

import Language.Haskell.TH
import Data.Text(Text, unpack, pack)
import Yesod
import Yesod.Admin.Helpers.Text


-- | Get an object associated with the given entity. The object
-- generated is undefined but useful for type kludges.

undefinedObjectOf :: PersistEntity v
                  => EntityField v typ
                  -> v
undefinedObjectOf _ = undefined

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
persistType :: Text
            -> TypeQ
            -> TypeQ
persistType entity backend = appT (conT n) backend
      where n = mkNameT $ camelCaseUnwords [ entity
                                           , "Generic"
                                           ]


-- | Generates the entities type alias name

entityAdmin :: String -- ^ the persistent entity
            -> String
entityAdmin entity = entity ++ "Admin"

getEntityAdmin :: String  -- ^ the persistent entity
               -> String
getEntityAdmin entity = "get" ++ entity ++ "Admin"

textL :: Text -> ExpQ
textL t = varE 'pack `appE` stringE (unpack t)

mkNameT :: Text -> Name
mkNameT = mkName . unpack

{-
defaultCons     :: Text -> Text  -- ^ creates a constructor name from
                                 -- a sentence.
defaultFunction :: Text -> Text  -- ^ creates a function name from a
                                 -- sentence.

defaultCons     = capitalise . camelCase
defaultFunction = unCapitalise . camelCase

defaultConsE    :: Text -> ExpQ  -- ^ Th version of defaultCons
defaultConsP    :: Text -> [PatQ] -> PatQ
                -- ^ TH version but for patterns

defaultConsE   = conE . mkNameT . defaultCons
defaultConsP t = conP . mkNameT $ defaultCons t

defaultFunctionE :: Text -> ExpQ
defaultFunctionE = varE . mkNameT . defaultFunction


defaultTitle  :: Text -> Text
defaultTitleE :: Text -> ExpQ

defaultTitle  = capitalise . unCamelCase
defaultTitleE = textL . defaultTitle

-}

-- | Create a single argument function
singleArgFunc :: Name            -- ^ The name of the function
              -> [(PatQ, ExpQ)]  -- ^ The clauses
              -> DecQ
singleArgFunc name consExp = funD name clauses
       where clauses = [ mkClause c e | (c,e) <- consExp ]
             mkClause c e = clause [c] (normalB e) []
