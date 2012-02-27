{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TemplateHaskell              #-}

{-|

Module to generate admin code for persistent entries.

-}

module Yesod.Admin.TH.Entity
       ( 
       -- * Admin section.
       -- $adminsection

       -- * Attribute naming and title.
       -- $attributeNameAndTitle

       -- * Attribute constructors.
       -- $attributeconstructors

       -- * Helper functions.
       -- $helpers
{-       
         AdminInterface(..)
       , simpleAdmin
       , mkYesodAdmin
       , mkEntityAdmin
       -- * Low level Template haskell functions. 
       -- $lowlevel
       
       , deriveAdministrable
       , deriveInlineDisplay
       , deriveAttributeDisplay
-}
       ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import Data.Maybe
import Language.Haskell.TH
import Database.Persist
import Yesod.Admin.Helpers
import Yesod.Admin.TH.Helpers
import Yesod.Admin.Class
import Yesod.Admin.Subsite

type Text = T.Text
pack = T.pack
unpack = T.unpack

-- $adminsection
-- To define the admin interface for a persistent objects one needs to
-- define and admin section in the persistent object definition For
-- example look at the definition of person in the following code.
--
-- >      [persist|
-- >                Person
-- >                        name    Text
-- >                        email   Text
-- >                        age     Text
-- >                        address Text
-- >                        Admin
-- >                            inline nameAndEmail
-- >                            list   Name Email Age
-- >                            title Name Full Name
-- >     |]
--
-- In the above code snippet, we have defined the /fields/ @inline@,
-- @list@ and @title@.
--
-- The allowed fields are:
--
-- [@inline@] Attribute used in the inline display of the object. Should
--    occur only once in the admin section. There should be a single
--    parameter which is the name of the attribute (See the convention on
--    attribute naming)
--
-- [@list@] A list of attributes used in the selection list display of
--    the objects. Should occur only once in the admin section. The
--    parameter is the list of attribute and the ordering of the
--    attribute should be as required in the selection listing.
--
-- [@plural@] The plural name for the object.
--
-- [@singular@] The singular name for the object.
--
-- [@title@] Used to override the default title of an attribute. The
--    first attribute is the attribute name and the rest of paramenters
--    form the title. There can be multiple title definitions one for
--    each attribute.


-- $attributeNameAndTitle
-- Attributes can be either
--
--   1. A string that starts with an lower case letter e.g. @name@
--   in which case it is one of the fields of the Persistent entity.
--
--   2. A string that starts with a upper case letter
--   e.g. @NameAndEmail@ in which case it denotes a function which
--   when applied to the objects returns the displayed string. The
--   function name is obtained by converting the first character of
--   the name into lower case (i.e @fooBar@ for an attribute @FooBar@).
--   The type of the function should be.
--
--   @('PersistEntity' v, 'PersistStore' b m) => v -> b m 'Text'@
--
--
-- The default attribute title for a attribute fooBarBiz is @\"Foo bar
-- biz\"@. I.e it is the uncamelcased version of the attribute name.  You
-- can override setting the title field in the Admin section of the
-- object.




-- $attributeconstructors
--
-- The TH code generates on constructor for each database entry plus
-- what ever constructed attributes are used in listings. The
-- constructors are the following.
--
--  1. For a database attribute it is represented by camel cased
--     concatenation of the entity name, the attribute name and the
--     string Attribute. For example the database column @name@ of
--     entity Person will give a constructor @PersonNameAttribute@
--
--  2. For a constructed attribute corresponding to a function, the
--     constructor will be the function name with the first letter
--     capitalised. For example if the function name is
--     @nameAndEmail@, the constructor will be @NameAndEmail@


{- $helpers

Besides declaring all the necessary admininstrative instances,
@`mkYesodAdmin`@ applied to the admin interfaces of entity Foo also
declares the following:

  1. The type alias @FooAdmin@ for the time @Admin Site Foo@
  
  2. The function @getFooAdmin@

This is to facilitate hooking of the admin subsite to the main
subsite. You can then include the route of the kind

> /admin/foo FooAdminR FooAdmin getFooAdmin

in your main routes file.


-}

-- | This datatype controls the admin site generate via the template
-- haskell functions of this module. Having defined this type you can
-- use either either `mkYesodAdmin` or `mkEntityAdmin` (if you want to
-- tweak the access controls).

data AdminSection
     = AdminSection { singular :: Text  -- ^ The singular name
                    , plural   :: Text  -- ^ The plural form
                    , attributeTitleOverride :: [(Text, Text)]
                       -- ^ List of tuples (c,t) where c is a
                       -- attribute and t is its title. You need to
                       -- specify only those attributes whose default
                       -- title you are not happy with
                    , inline   :: Text
                       -- ^ How to display the inline display of the
                       -- object. It could either be a database column
                       -- (a capitalised name) or a constructed (a
                       -- name that starts with lower case).
                    , list     :: [Text]
                       -- ^ Ordered list of attributes in the
                       -- selection listing of the object.
                    }


{-

-- | Generate a simple admin interface. The argument follows the same
-- convention as that of an administrative attribute. It can either be
-- the name of a function that returns the inline representation or
-- can be one of the database column, in which case the first
-- character in upper case.
simpleAdmin :: String          -- ^ How to display inline
            -> AdminInterface v
simpleAdmin col = AdminInterface { singular = ""
                                 , plural   = ""
                                 , inline   = col
                                 , listing  = []
                                 , attributeTitleOverride = []
                                 }

-- | This function is similar to the 'mkYesodAdmin' function but does
-- not derive a 'YesodAdmin' instance for the given entity. Use this
-- function if you want to have a different access control policy than
-- what is provided by the default 'YesodAdmin' instance.

mkEntityAdmin :: PersistEntity v
              => String        -- ^ Name of the foundation type
              -> AdminInterface v
              -> DecsQ
mkEntityAdmin site ai = do insts <- sequence [ deriveAdministrable' ai
                                             , deriveInlineDisplay'  site ai
                                             , deriveAttributeDisplay' site ai
                                             ]
                           aliases <- defEntityAliases site entity
                           return (insts ++ aliases)
       where entity = typeName $ getObject ai

-- | Given the name of the foundation type and admin interface for a
-- persistent type, this function derives all the necessary class
-- instances that are required create the admin site for this type. It
-- also defines the following aliases. If the entity has name @Foo@
-- then it also splices the declarations:
--
-- > type FooAdmin = Admin Site Foo
-- > getFooAdmin :: Site -> FooAdmin
-- > getFooAdmin _ = getAdmin
--
-- The above declaration is to facilitate hooking the admin site of
-- Foo to the main site. Further it also derives the default
-- 'YesodAdmin' instance where only super user has access to the admin
-- facilities. If you want to configure the access controls explicitly
-- then use the 'mkEntityAdmin' function instead and code up the
-- 'YesodAdmin' instance by hand.

mkYesodAdmin :: PersistEntity v
             => String            -- ^ Name of the foundation type
             -> AdminInterface v  -- ^ The admin interface
             -> DecsQ
mkYesodAdmin site ai = do inst <- mkEntityAdmin site ai
                          yaInst <- yadminInst
                          return $ inst ++ [yaInst]
  where yadminInst = mkInstance [] ''YesodAdmin [siteType, tyType] []
        siteType   = conT $ mkName site
        tyType     = conT $ mkName $ typeName $ getObject ai

-- $lowlevel
--
-- You will most likely not need these functions but in case you want
-- to have more control on the generated haskell code you can use
-- these.

                     
-- | Derive an instance of @`Administrable`@ for the type @v@ given
-- the AdminInterface for @v@.
deriveAdministrable  :: PersistEntity v
                     => AdminInterface v
                     -> DecsQ
deriveAdministrable' :: PersistEntity v
                     => AdminInterface v
                     -> DecQ
deriveAdministrable  = fmap (:[]) . deriveAdministrable'
deriveAdministrable' ai = mkInstance [] ''Administrable [persistType v] instBody
      where v        = getObject ai
            ats      = attributes ai
            instBody = singularPlural ai
                     ++ [ defAttribute v ats
                        , defAttributeTitle v ats $ attributeTitleOverride ai
                        , defListAttributes v $ listing ai
                        ]

-- | Derive an instance of `InlineDisplay` for the type `v` given the
-- AdminInterface for `v`.
deriveInlineDisplay  :: PersistEntity v
                     => String            -- ^ the site's foundation type
                     -> AdminInterface v  -- ^ the administrative interface
                     -> DecsQ

-- | Same as `deriveInlineDisplay` but does not wrap the declaration
-- inside a list. Not very useful in the wild as splicing expects
-- `DecsQ` instead `DecQ` but useful in defining other template
-- haskell function. Currently not exported.

deriveInlineDisplay' :: PersistEntity v
                     => String
                     ->AdminInterface v
                     -> DecQ
deriveInlineDisplay site = fmap (:[]) . deriveInlineDisplay' site
deriveInlineDisplay' site ai
                     = mkInstance [] 
                          ''InlineDisplay [sub, siteT, persistType v] instBody
     where sub    = varT $ mkName "sub"
           siteT  = conT $ mkName site
           v = getObject ai
           body = normalB $ displayRHS v $ inline ai
           instBody = [valD (varP 'inlineDisplay) body []]

-- | Derive an instance of `AttributeDisplay` for the type `v` given
-- the AdminInterface for `v`.
deriveAttributeDisplay :: PersistEntity v
                    => String             -- ^ the site's foundation type
                    -> AdminInterface v   -- ^ the administrative interface
                    -> DecsQ

-- | Same as `deriveAttributeDisplay` but does not wrap the
-- declaration inside a list. Not very useful in the wild as splicing
-- expects `DecsQ` instead `DecQ` but useful in defining other
-- template haskell function. Currently not exported

deriveAttributeDisplay' :: PersistEntity v
                        => String        -- ^ the site's foundation type
                        -> AdminInterface v -- ^ the administrative interface
                        -> DecQ
deriveAttributeDisplay site = fmap (:[]) . deriveAttributeDisplay' site
deriveAttributeDisplay' site ai
            = mkInstance []
                    ''AttributeDisplay [sub, siteT, persistType v]
                     [ defAttributeDisplay v $ attributes ai]
     where sub   = varT $ mkName "sub"
           siteT = conT $ mkName site
           v     = getObject ai


-- | The TH code @defAdmin Site Foo@ generates the following
-- declarations
-- 
-- > type FooAdmin = Admin Site Foo
-- > getFooAdmin :: Site -> FooAdmin
-- > getFooAdmin _ = getAdmin
--

defEntityAliases :: String -- ^ the site's foundation type
                 -> String -- ^ the persistent entity
                 -> DecsQ
defEntityAliases site entity = sequence [ tySynD alias [] admin
                                        , sigD funcName funcType
                                        , funD funcName [body]
                                        ]
     where admin       = conT ''Admin `appT` siteTy
                                      `appT` entityTy
           alias       = mkName $ entityAdmin entity
           funcType    = arrowT `appT` siteTy
                                 `appT` conT alias
           funcName    = mkName $ getEntityAdmin entity
           body        = clause [wildP] (normalB  $ varE 'getAdmin) []
           siteTy    = conT $ mkName site   -- the TH type of site
           entityTy  = conT $ mkName entity -- the TH type of the entity

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

-- | Define the 'listAttribute' member.
defListAttributes  :: PersistEntity v
                => v
                -> [String]
                -> DecQ
defListAttributes v ats = valD (varP 'listAttributes) body []
    where cons = map (conE . mkName) $ map (constructor v) ats
          body = normalB $ listE cons


-- | Define the attribute data type.
defAttribute :: PersistEntity v
             => v
             -> [String]
             -> DecQ
defAttribute v ats = dataInstD (cxt []) ''Attribute [persistType v]
                               (map mkConQ cons) []
     where cons = map (constructor v) ats
           mkConQ = flip normalC [] . mkName 


defAttributeFunc :: Name          -- ^ The name of the function
             -> [(String, ExpQ)]  -- ^ The constructors
             -> DecQ
defAttributeFunc name consExp = funD name clauses
       where clauses = [ mkClause c e | (c,e) <- consExp ]
             mkClause c e = clause [cP] (normalB e) []
                      where cP = conP (mkName c) []

defAttributeTitle :: PersistEntity v
                  => v
                  -> [ String ]
                  -> [(String,String)]
                  -> DecQ
defAttributeTitle v ats override = defAttributeFunc 'attributeTitle 
                                                    $ map titleExp ats
    where titleExp at = (constructor v at, litE . stringL $ getTitle at)
          getTitle at = fromMaybe (capitalise $ unCamelCase at) 
                                  $ lookup at override
defAttributeDisplay :: PersistEntity v
                    => v
                    -> [String]
                    -> DecQ
defAttributeDisplay v =  defAttributeFunc 'attributeDisplay . map colDisplay 
    where colDisplay at = (constructor v at, displayRHS v at)


getObject :: PersistEntity v
          => AdminInterface v
          -> v
getObject _ = undefined

attributes :: PersistEntity v
        => AdminInterface v
        -> [String]
attributes ai = nub (listing ai ++ dbCols)
   where v       = getObject ai
         dbCols  = map (capitalise . columnName) $ entityColumns $ entityDef v

isDBAttribute :: String -> Bool
isDBAttribute = isUpper . head

constructor :: PersistEntity v => v -> String -> String
constructor v col | isDBAttribute col = capitalise $ camelCase
                                                $ unwords [ typeName v
                                                          , col
                                                          , "Attribute"
                                                          ]
                  | otherwise      = capitalise col

displayRHS :: PersistEntity v
           => v
           -> String
           -> ExpQ
displayRHS v at | isDBAttribute at
                                 = let fname = varE $ mkName $ fieldName v at
                                       in [| inlineDisplay . $fname |]
                | otherwise      = varE $ mkName at

-}
