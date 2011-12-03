{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE ExistentialQuantification    #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}

{-|

This module contains the template haskell routines to build admin
interfaces.

-}

module Yesod.Admin.TH
       ( AdminColumn
       , AdminInterface(..)
       , deriveAdministrable
       , deriveInlineDisplay
       , field
       , constructed
       , (<:>)
       ) where

import Data.Text (Text, pack, empty)
import Data.Char
import Data.List
import Language.Haskell.TH
import Database.Persist
import Database.Persist.Base
import Yesod.Admin.Helpers
import Yesod.Admin.TH.Helpers
import Yesod.Admin.Class

-- | Datatype used to generate the columns of an administrable object.
data  AdminColumn v = Field         String String
                    | Constructed   String String


-- | Create an admin column associated with an entity field. The title
-- name will be the uncamel cased version of the column name.
field :: PersistEntity v => String -> AdminColumn v

-- | Creates a constructed column name which will be 
constructed :: String -> AdminColumn v
constructed func = Constructed (capitalise $ unCamelCase func) func

field colName = Field (capitalise $ unCamelCase colName) colName

(<:>) :: String -> AdminColumn v -> AdminColumn v
(<:>) title (Field _ cname)         = Field title cname
(<:>) title (Constructed _ func) = Constructed title func


-- | To define an admin site for a type one needs to define instances
-- of `Administrable`, `InlineDisplay`, and `ColumnDisplay` besides
-- the `YesodAdmin` instance. This module exports template haskell
-- functions like `mkAdmin` which expect an `AdminInterface` value as
-- an argument. Therefore the first step in making use of the template
-- haskell functions of this module is to define the `AdminInterface`
-- for the appropriate type.

data AdminInterface v
     = AdminInterface { singular :: String           -- ^ The singular name
                      , plural   :: String           -- ^ The plural form
                      , inline   :: AdminColumn v    -- ^ What column to
                                                     -- use to display
                                                     -- inline
                      , listing  :: [AdminColumn v]  -- ^ Columns to be
                                                     -- listed and in
                                                     -- what order.
                      }

getObject :: PersistEntity v
          => AdminInterface v
          -> v
getObject _ = undefined
getObjectFromCol :: PersistEntity v
                 => AdminColumn v
                 -> v
getObjectFromCol _ = undefined

colConstructor :: PersistEntity v
               => AdminColumn v
               -> String
colConstructor c@(Field _ s) = let name = typeName $ getObjectFromCol c
                               in capitalise $ camelCase $ unwords [ name
                                                                   , s
                                                                   , "Column"
                                                                   ]
colConstructor (Constructed _ s) = capitalise s


colConstructors :: PersistEntity v
                => [AdminColumn v]
                -> [String]
colConstructors cols = map colConstructor cols

defColumn :: PersistEntity v
          => AdminInterface v
          -> DecQ
defColumn ai = dataInstD (cxt []) ''Column [typ] (map mkConQ cons) []
     where b = varT $ mkName "b"
           cons = colConstructors $ listing ai
           typ = persistType (getObject ai) b
           mkConQ = flip normalC [] . mkName 


mkListing  :: PersistEntity v
           => AdminInterface v
           -> DecQ
mkListing ai = valD (varP 'listColumns) body []
    where cons = map (conE . mkName) $ colConstructors $ listing ai
          body = normalB $ listE cons
singularPlural :: PersistEntity v
               => AdminInterface v
               -> [DecQ]

singularPlural ai = defun 'objectSingular (singular ai) ++
                       defun 'objectPlural   (plural ai)
                     
    where   defun _    []  = []
            defun name str = [funD name $ [rhs str]]
            rhs str = clause [wildP] (normalB $ litE $ stringL str) []

mkColumnTitle :: PersistEntity v
              => AdminInterface v
              -> DecQ
mkColumnTitle ai = mkColumnFunc ai 'columnTitle (litE . stringL . getTitle)
    where getTitle (Field t _) = t
          getTitle (Constructed t _) = t

mkColumnFunc :: PersistEntity v
             => AdminInterface v
             -> Name
             -> (AdminColumn v -> ExpQ)
             -> DecQ
mkColumnFunc ai name bodyfunc = funD name clauses
       where cons    = colConstructors $ listing ai
             bodies  = map bodyfunc $ listing ai
             clauses = zipWith mkClause cons bodies
             mkClause c b = clause [cP] (normalB b) []
                      where cP = conP (mkName c) []

deriveAdministrable :: PersistEntity v
                    => AdminInterface v
                    -> DecQ
deriveAdministrable ai = mkInstance [] ''Administrable [vtype] instBody
      where vtype = persistType (getObject ai) $ varT $ mkName "b"
            instBody = singularPlural ai
                     ++ [ mkListing ai
                        , defColumn ai
                        , mkColumnTitle ai
                        ]

displayRHS :: PersistEntity v
           => v
           -> AdminColumn v
           -> ExpQ
displayRHS v (Field _ name) = let fname = varE $ mkName $ fieldName v name
                              in [| inlineDisplay . $fname |]
displayRHS _ (Constructed _ name) = varE $ mkName name
           
deriveInlineDisplay :: PersistEntity v
                    => AdminInterface v
                    -> DecQ
deriveInlineDisplay ai = mkInstance [monadP m, persistBackendP b m]
                         ''InlineDisplay [b, m, persistType v b] instBody
     where b = varT $ mkName "b"
           m = varT $ mkName "m"
           v = getObject ai
           body = normalB $ displayRHS v $ inline ai
           instBody = [valD (varP 'inlineDisplay) body []]
