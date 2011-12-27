{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TemplateHaskell              #-}

{-|

Template haskell functions to generate admin related declarations for
persistent entities. Here is an example of how to use the functions
here. Assume a persistent entry of the following kind.

>      [persist|
>                Person
>                        name    Text
>                        email   Text
>                        age     Text
>                        address Text
>     |]

You can define the admin interfaces by the following code


>
> mkYesodAdmin (simpleAdmin "nameAndEmail" :: AdminInterface Person)
> nameAndEmail person = return $ Text.concat [ personName person
>                                            , "<", personEmail person, ">"
>                                            ]

The above code will generate an admin instance where the function
@nameAndEmail@ is used to display the objects in inline mode. Further
the listings of Person will have one field with title Person and
entries obtained by applying nameAndEmail. For more elaborate
configuration check the 'AdminInterface' datatype.

-}

module Yesod.Admin.TH.Entity
       ( 
       -- * Displaying objects.
       -- $display

       -- * Column titles.
       -- $columntitle

       -- * Column constructors.
       -- $columnconstructors
       
       AdminInterface(..)
       , simpleAdmin
       , deriveAdministrable
       , deriveInlineDisplay
       , deriveColumnDisplay
{-
       , mkAdminInstances
       , mkYesodAdmin
-}
       ) where

import Data.Text (Text, pack, empty)
import Data.Char
import Data.List
import Data.Maybe
import Language.Haskell.TH
import Database.Persist
import Database.Persist.Base
import Yesod.Admin.Helpers
import Yesod.Admin.TH.Helpers
import Yesod.Admin.Class
import Yesod.Admin.Subsite


-- $display
--
-- In an admin site the object is either displayed in inline text or
-- in listings. The field 'inline' of the 'AdminInterface' datatype
-- controls the inline display of the objects where as 'listing'
-- controls what columns are shown and in which order.  Both 'inline'
-- or an element of 'listing' can be either:
--
--   1. A string that starts with an upper case letter e.g. @\"Name\"@
--   in which case it denotes a database column or
--
--   2. A string that starts with a lower case letter
--   e.g. @\"nameAndEmail\"@ in which case it denotes a function which
--   when applied to the objects returns the displayed string.  Note
--   that the function should have type 
--
-- @('PersistEntity' v, 'PersistBackend' b m) => v -> b m 'Text'@
--
--


-- $columntitle
--
-- The default column title for a column fooBarBiz is @\"Foo bar
-- biz\"@. I.e it is the uncamelcased version of the column name.  You
-- can override the title by changing the 'columnTitleOverride' field
-- of the 'AdminInterface'.
--


-- $columnconstructors
-- The TH code generates on constructor for each database entry plus what
-- ever constructed columns are used in listings. The constructors are the
-- following. 
--
--  1. For a database column it is represented by camel cased concatnation
--  of the entity name, the column name and the string Column. For example
--  the database column @name@ of entity Person will give a constructor
--  @PersonNameColumn@
--
--  2. For a constructed column corresponding to a function, the constructor
--  will be the function name with the first letter capitalised. For example
--  if the function name is @nameAndEmail@, the constructor will be
--  @NameAndEmail@


-- | This datatype controls the admin site generate via the template
-- haskell functions of this module. Having defined this type you can
-- use either either `mkYesodAdmin` or `mkAdminInstances` (if you want
-- to tweak the access controls).

data AdminInterface v
     = AdminInterface { singular :: String  -- ^ The singular name
                      , plural   :: String  -- ^ The plural form
                      , columnTitleOverride :: [(String, String)]
                             -- ^ List of tuples (c,t) where c is a
                             -- column and t is its title. You need to
                             -- specify only those columns whose
                             -- default title you are not happy with
                      , inline   :: String
                             -- ^ How to display the inline display of
                             -- the object. It could either be a
                             -- database column (a capitalised name)
                             -- or a constructed one (a name that
                             -- starts with lower case).
                      , listing  :: [ String ]
                             -- ^ Ordered list of the columns in the
                             -- listing of the object.
                      }

-- | Generate a simple admin interface. The argument follows the same
-- convention as that of an administrative column. It can either be
-- the name of a function that returns the inline representation or
-- can be one of fields, with the first character in upper case.
simpleAdmin :: String          -- ^ How to display inline
            -> AdminInterface v
simpleAdmin col = AdminInterface { singular = ""
                                 , plural   = ""
                                 , inline   = col
                                 , listing  = []
                                 , columnTitleOverride = []
                                 }
                     
-- | Derive an instance of `Administrable` for the type `v` given the
-- AdminInterface for `v`.
deriveAdministrable :: PersistEntity v
                    => AdminInterface v
                    -> DecQ
deriveAdministrable ai = mkInstance [] ''Administrable [vtype] instBody
      where vtype   = persistType v $ varT $ mkName "b"
            v       = getObject ai
            cols    = columns ai
            instBody = singularPlural ai
                     ++ [ defListColumns v $ listing ai
                        , defColumn v cols
                        , defColumnTitle v cols $ columnTitleOverride ai
                        ]

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

-- | Derive an instance of `ColumnDisplay` for the type `v` given the
-- AdminInterface for `v`.
deriveColumnDisplay :: PersistEntity v
                    => AdminInterface v
                    -> DecQ
deriveColumnDisplay ai = mkInstance [monadP m, persistBackendP b m]
                         ''ColumnDisplay [b, m, persistType v b]
                         [ defColumnDisplay v $ columns ai]
     where b = varT $ mkName "b"
           m = varT $ mkName "m"
           v = getObject ai


-- | TH function to generate the definition of members
-- 'objectSingular' and 'objectPlural'.
singularPlural :: PersistEntity v
               => AdminInterface v
               -> [DecQ]
singularPlural ai = defun 'objectSingular (singular ai) ++
                       defun 'objectPlural   (plural ai)
    where   defun _    []  = []
            defun name str = [funD name $ [rhs str]]
            rhs str = clause [wildP] (normalB $ litE $ stringL str) []

-- | Define the 'listColumn' member.
defListColumns  :: PersistEntity v
                => v
                -> [String]
                -> DecQ
defListColumns v cols = valD (varP 'listColumns) body []
    where cons = map (conE . mkName) $ map (constructor v) cols
          body = normalB $ listE cons


-- | Define the column data type.
defColumn :: PersistEntity v
          => v
          -> [String]
          -> DecQ
defColumn v cols = dataInstD (cxt []) ''Column [typ] (map mkConQ cons) []
     where b    = varT $ mkName "b"
           cons = map (constructor v) cols
           typ  = persistType v b
           mkConQ = flip normalC [] . mkName 


defColumnFunc :: Name              -- ^ The name of the function
             -> [(String, ExpQ)]  -- ^ The constructors
             -> DecQ
defColumnFunc name consExp = funD name clauses
       where clauses = [ mkClause c e | (c,e) <- consExp ]
             mkClause c e = clause [cP] (normalB e) []
                      where cP = conP (mkName c) []

defColumnTitle :: PersistEntity v
              => v
              -> [ String ]
              -> [(String,String)]
              -> DecQ
defColumnTitle v cols override = defColumnFunc 'columnTitle $ map titleExp cols
    where titleExp col = (constructor v col, litE . stringL $ getTitle col)
          getTitle col = fromMaybe (unCamelCase col) $ lookup col override

defColumnDisplay v =  defColumnFunc 'columnDisplay . map colDisplay 
    where colDisplay col = (constructor v col, displayRHS v col)


getObject :: PersistEntity v
          => AdminInterface v
          -> v
getObject _ = undefined

columns :: PersistEntity v
        => AdminInterface v
        -> [String]
columns ai = nub (listing ai ++ dbCols)
   where v       = getObject ai
         dbCols  = map columnName $ entityColumns $ entityDef v

isDBColumn :: String -> Bool
isDBColumn = isUpper . head

constructor :: PersistEntity v => v -> String -> String
constructor v col | isDBColumn col = capitalise $ camelCase
                                                $ unwords [ typeName v
                                                          , col
                                                          , "Column"
                                                          ]
                  | otherwise      = capitalise col

displayRHS :: PersistEntity v
           => v
           -> String
           -> ExpQ
displayRHS v col | isDBColumn col = let fname = varE $ mkName $ fieldName v col
                                        in [| inlineDisplay . $fname |]
                 | otherwise      = varE $ mkName col
