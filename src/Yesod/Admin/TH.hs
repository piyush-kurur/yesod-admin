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
