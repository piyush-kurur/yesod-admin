{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TemplateHaskell              #-}

{-|

Template haskell functions to generate admin related declarations for
persistent entities. To be able to administer an entity you need to
declare.

     1. An 'Admininstrable' instance

     2. An 'InlineDisplay' instance

     3. An 'ColumnDisplay'  instance and finaly

     4. A  'YesodAdmin' instance.

This module proved TH code for generating all these. You need to
define an 'AdminInterface' for your type and then use the
'mkYesodAdmin' function.


-}

module Yesod.Admin.TH.Entity
       ( AdminColumn
       , field
       , constructed
       , (<:>)
       , AdminInterface(..)
       , deriveAdministrable
       , deriveInlineDisplay
       , deriveColumnDisplay
       , mkAdminInstances
       , mkYesodAdmin
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
import Yesod.Admin.Subsite

-- | Datatype used to generate the columns of an administrable object.
data  AdminColumn v = Field         String String
                    | Constructed   String String


-- | Create an admin column associated with an entity field. The title
-- name will be the uncamel cased version of the column name.
field :: PersistEntity v => String -> AdminColumn v
field colName = Field (capitalise $ unCamelCase colName) colName

-- | Columns of the type need not directly correspond to data base
-- columns and can also be a constructed one. One can create a
-- computed field as @'constructed' \"nameAndEmail\"@, provided there
-- is a function @nameAndEmail@ which has type @'PersistBackend' b m
-- => v -> b m 'Text'@. The title of this column would then be @\"Name
-- and email\"@
constructed :: String           -- ^ name of a function of type 
                                -- @v -> b m 'Text'@.
            -> AdminColumn v
constructed func = Constructed (capitalise $ unCamelCase func) func


-- | Set the title of the given column. The default column title for a
-- field defined @'field' \"name\"@ is @\"Name\"@. To use @\"Full
-- Name\"@, use the following: @\"Full Name\" '<:>' 'field' \"name\"@.

(<:>) :: String -> AdminColumn v -> AdminColumn v
(<:>) title (Field _ cname)         = Field title cname
(<:>) title (Constructed _ func) = Constructed title func


-- | To define an admin site for a type one needs to define instances
-- of `Administrable`, `InlineDisplay`, and `ColumnDisplay` besides
-- the `YesodAdmin` instance. This module exports template haskell
-- functions like `mkYesodAdmin` which expect an `AdminInterface`
-- value as an argument. Therefore the first step in making use of the
-- template haskell functions of this module is to define the
-- `AdminInterface` for the appropriate type.

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

-- | The function always return the undefined object of type `v` give
-- an admin interface for it. Often in template haskell function on
-- needs to get things like type name or column name of the given only
-- the admin interface for `v`. The idea is to use getObject to get
-- the corresponding object and then use functions like entityDef
-- etc. 

getObject :: PersistEntity v
          => AdminInterface v
          -> v
getObject _ = undefined

-- | A version of getObject where only admin column is given.

getObjectFromCol :: PersistEntity v
                 => AdminColumn v
                 -> v
getObjectFromCol _ = undefined

-- | The column constructor for the given admin column.

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

-- | define the column data type.

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

-- | Derive an instance of `Administrable` for the type `v` given the
-- AdminInterface for `v`.
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

-- | Derive an instance of `InlineDisplay` for the type `v` given the
-- AdminInterface for `v`.
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

mkColumnDisplay v ai =  mkColumnFunc ai 'columnDisplay $ displayRHS v

-- | Derive an instance of `ColumnDisplay` for the type `v` given the
-- AdminInterface for `v`.
deriveColumnDisplay :: PersistEntity v
                    => AdminInterface v
                    -> DecQ
deriveColumnDisplay ai = mkInstance [monadP m, persistBackendP b m]
                         ''ColumnDisplay [b, m, persistType v b]
                         [ mkColumnDisplay v ai ]
     where b = varT $ mkName "b"
           m = varT $ mkName "m"
           v = getObject ai

-- | This combinator derives all the basic instances like
-- `InlineDisplay`, `ColumnDisplay` and `Administrable` classes. You
-- need to use this function if you want to have a different access
-- control policy than what is provieded by the default `YesodAdmin`
-- instance. If the default instance of `YesodAdmin` suffices use the
-- `mkYesodAdmin` combinator instead.

mkAdminInstances :: PersistEntity v
                 => AdminInterface v
                 -> DecsQ
mkAdminInstances ai = sequence [ deriveAdministrable ai
                               , deriveInlineDisplay ai
                               , deriveColumnDisplay ai
                               ]

-- | Given the name of the foundation type and admin interface for a
-- persistent type, this function derives all the necessary class
-- instances that are required create the admin site for this
-- type. The `YesodAdmin` instance derived is the default one where
-- only super user has access to the admin facility. If you want to
-- configure the access controls explicitly then use the
-- mkAdminInstances function instea and code up the `YesodAdmin`
-- instance by hand.

mkYesodAdmin :: PersistEntity v
             => String            -- ^ Name of the foundation type
             -> AdminInterface v  -- ^ The admin interface
             -> DecsQ
mkYesodAdmin site ai = do inst <- mkAdminInstances ai
                          yaInst <- yadminInst
                          return $ inst ++ [yaInst]
  where yadminInst = mkInstance [] ''YesodAdmin [siteType, tyType] []
        siteType   = conT $ mkName site
        tyType     = conT $ mkName $ typeName $ getObject ai
