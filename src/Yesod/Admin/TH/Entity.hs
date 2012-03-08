{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE OverloadedStrings            #-}
{-|

Module to generate admin code for persistent entries.

-}

module Yesod.Admin.TH.Entity
       (
       -- * Admin section.
       -- $adminsection

       -- * Attribute naming
       -- $attributeNameAndTitle

       -- * Attribute title and constructors.
       -- $attributetitleandconstructors

       -- * Helper functions.
         AdminInterface(..)
       , mkAdminClasses
       -- * Low level Template haskell functions.
       -- $lowlevel
       , entityDefToInterface
       , deriveAdministrable
       , deriveInlineDisplay
       , deriveAttributeDisplay
       ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Language.Haskell.TH
import Database.Persist.EntityDef
import Yesod.Admin.Helpers.Text
import Yesod.Admin.TH.Helpers
import Yesod.Admin.Class
import Yesod.Admin.Subsite

type Text = T.Text
type Map = M.Map

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
-- >                            inline NameAndEmail
-- >                            list   name email age
-- >                            title name Full Name
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
--    attribute naming). The default value is the first field in the entity
--    definition.
--
-- [@list@] A list of attributes used in the selection list display of
--    the objects. Should occur only once in the admin section. The
--    parameter is the list of attribute and the ordering of the
--    attribute should be as required in the selection listing. The
--    default value is to use the same field as the inline display.
--
-- [@plural@] The plural name for the object.
--
-- [@show@] The list of attributes that are shown on the read page of
--    the object. This defaults to all the dbAttributes.
--
-- [@sort@]. Controls how the values are sorted on the selection page.
--    The values should be dbEntries and nothing else. By default the
--    sorting is done based on the default sort associated with that
--    entry.
--
-- [@singular@] The singular name for the object.
--
-- [@title@] Used to override the default title of an attribute. The
--    first attribute is the attribute name and the rest of
--    paramenters form the title. There can be multiple title
--    definitions one for each attribute. The default title is the
--    uncamelcased version of the attribute name. For example
--    @fullName@ and @NameAndEmail@ have default titles as @\"Full
--    name\"@ and @\"Name and email\"@ respectively.

-- | This datatype controls the admin site generate via the template
-- haskell functions of this module. Having defined this type you can
-- use either either `mkYesodAdmin` or `mkEntityAdmin` (if you want to
-- tweak the access controls).

data AdminInterface
     = AdminInterface { name         :: Text  -- ^ Name of the entity
                      , singular     :: Maybe Text   -- ^ The singular
                                                     -- name
                      , plural       :: Maybe Text   -- ^ The plural form
                      , dbAttrs      :: [Text]
                      , derivedAttrs :: [Text]
                      , titles     :: Map Text Text
                                   -- ^ The titles of each attribute.
                                   -- ^ The key is attribute
                                   -- constructor name and the value
                                   -- is the title of the attribute.
                      , inline     ::  Text
                      -- ^ How to display the inline display of the
                      -- object. It could either be a database column
                      -- (a name that starts with lower case) or a
                      -- constructed (a name that starts with upper
                      -- case).
                      , readPageAttrs :: Maybe [Text]
                      -- ^ Attributes constructors to be shown on the
                      -- read page.
                      , selectionPageAttrs  :: Maybe [Text]
                      -- ^ Ordered list of attribute constructors in
                      -- the selection listing of the object.
                      } deriving Show

defaultInterface :: EntityDef -> AdminInterface
defaultInterface ed
   = AdminInterface { name     = en
                    , singular = Nothing
                    , plural   = Nothing
                    , dbAttrs  = dc
                    , derivedAttrs = []
                    , titles   = M.fromList ts
                    , inline   = head fs
                    , readPageAttrs      = Nothing
                    , selectionPageAttrs = Nothing
                    }
    where en = entityName ed
          dc = map (constructor en) fs
          fs = map (unHaskellName . fieldHaskell) $ entityFields ed
          ts = zip dc $ map titleOf fs

-- $attributetitleandconstructors
--
-- The default attribute title for a attribute fooBarBiz is @\"Foo bar
-- biz\"@. I.e it is the uncamelcased version of the attribute
-- name. You can override setting the title field in the Admin section
-- of the object.

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
--

isDerived :: Text -> Bool
isDerived = isUpper . T.head


titleOf       :: Text  -- ^ Attribute name
              -> Text
constructor   :: Text    -- ^ Entity name
              -> Text    -- ^ Attribute name
              -> Text

titleOf = capitalise . unCamelCase
constructor en attr
   | T.null    attr    = T.empty
   | isDerived attr    = capitalise attr
   | otherwise         = capitalise $ camelCaseUnwords [ en
                                                       , attr
                                                       , "Attribute"
                                                       ]

-- FIXME: Write a Quick check test to check dbAttrToFieldName
-- (constructer e x) = x where ever x starts with a lower case
-- alphabet.

dbAttrToFieldName t = unCapitalise $ camelCaseUnwords $ 
                      init $ tail $ unCamelCaseWords t


entityDefToInterface :: EntityDef -> AdminInterface
entityDefToInterface ed = ai { titles = M.union (titles ai) defTitles }
   where ai        = procAdminSection ed
         derived   = derivedAttrs ai
         defTitles = M.fromList $ zip derived $ map titleOf derived

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

-- | This TH combinator derives the three classes @`Administrable`@
-- @`InlineDisplay`@ and @`AttributeDisplay`@ for a persistent entity.

mkAdminClasses :: [EntityDef] -> DecsQ
mkAdminClasses = sequence . concatMap mapper
    where mkAC ai = [ deriveAdministrable' ai
                    , deriveInlineDisplay' ai
                    , deriveAttributeDisplay' ai
                    ]
          mapper   = mkAC . entityDefToInterface
          

-- | Derive an instance of @`Administrable`@ for the type @v@ given
-- the AdminInterface for @v@.

deriveAdministrable  :: AdminInterface
                     -> DecsQ
deriveAdministrable' :: AdminInterface
                     -> DecQ
deriveAdministrable  = fmap (:[]) . deriveAdministrable'

-- FIXME: Find a way to set the listSort option.
deriveAdministrable' ai = mkInstance [] ''Administrable [persistType en b]
                                     $ instBody
      where b        = varT $ mkName "b"
            en       = name ai
            instBody = [ defDBAttrs   ai
                       , defAttribute ai   b
                       , defAttributeTitle ai
                       ]
                       ++ catMaybes [ defObjectSingular ai
                                    , defObjectPlural   ai
                                    , defSelectionAttrs ai
                                    , defReadAttrs      ai
                                    ]

-- | Derive an instance of `InlineDisplay` for an entity give its
-- administrative interface.

deriveInlineDisplay  :: AdminInterface
                     -> DecsQ
-- | Similar to `deriveInlineDisplay` but does not wrap the
-- declaration inside a list. Not very useful in the wild as splicing
-- expects `DecsQ` instead `DecQ` but useful in defining other
-- template haskell function. Currently not exported

deriveInlineDisplay' :: AdminInterface
                     -> DecQ
deriveInlineDisplay  = fmap (:[]) . deriveInlineDisplay'
deriveInlineDisplay' ai = 
               mkInstance [persistStoreP b m]
                          ''InlineDisplay [b, m, persistType en b]
                          instBody
     where b     = varT $ mkName "b"
           m     = varT $ mkName "m"
           en    = name ai
           body = normalB $ displayRHS en $ inline ai
           instBody = [valD (varP 'inlineDisplay) body []]

-- | Derive an instance of `AttributeDisplay` for an entity give its
-- administrative interface.

deriveAttributeDisplay :: AdminInterface
                       -> DecsQ
deriveAttributeDisplay = fmap (:[]) . deriveAttributeDisplay'

-- | Same as `deriveAttributeDisplay` but does not wrap the
-- declaration inside a list. Not very useful in the wild as splicing
-- expects `DecsQ` instead `DecQ` but useful in defining other
-- template haskell function. Currently not exported

deriveAttributeDisplay' :: AdminInterface
                        -> DecQ
deriveAttributeDisplay' ai
            = mkInstance [persistStoreP b m]
                    ''AttributeDisplay [b, m, persistType en b]
                    instBody
     where b  = varT $ mkName "b"
           m  = varT $ mkName "m"
           en = name ai
           ats = map dbAttrToFieldName (dbAttrs ai) ++ derivedAttrs ai
           instBody = [ funD 'attributeDisplay $ map mkClause ats ]
           mkClause at = clause [constructorP en at] body []
                    where body = normalB $ displayRHS en at


displayRHS :: Text
           -> Text
           -> ExpQ
displayRHS en at | isDerived at = varE $ mkNameT $ unCapitalise at
                 | otherwise    = let fname = varE $ mkNameT $ fieldName en at
                                      in [| inlineDisplay . $fname |]


defObjectSingular :: AdminInterface -> Maybe DecQ
defObjectPlural   :: AdminInterface -> Maybe DecQ

defObjectSingular = fmap (textFun 'objectSingular) . singular
defObjectPlural   = fmap (textFun 'objectPlural) . plural


-- | Define the attribute data type.
defAttribute :: AdminInterface      -- ^ Entity name
             -> TypeQ               -- ^ Backend
             -> DecQ

defAttribute ai b = dataInstD (cxt []) ''Attribute [persistType en b]
                              cons [''Eq, ''Enum, ''Bounded]
    where cons   = map  mkConQ cs
          en     = name ai
          mkConQ = flip normalC [] . mkNameT
          cs     = M.keys $ titles ai

defAttributeTitle :: AdminInterface
                  -> DecQ
defAttributeTitle ai = singleArgFunc 'attributeTitle
                               $ [ (mkC c, textL t) | (c,t) <- cts]
    where cts   = M.toList $ titles ai
          mkC c = conP (mkNameT c) []



defSelectionAttrs :: AdminInterface -> Maybe DecQ
defReadAttrs      :: AdminInterface -> Maybe DecQ

defSelectionAttrs = fmap (defListVar 'selectionPageAttributes)
                         . selectionPageAttrs

defReadAttrs      = fmap (defListVar 'readPageAttributes)
                         . readPageAttrs

defDBAttrs        :: AdminInterface -> DecQ
defDBAttrs        = defListVar 'dbAttributes . dbAttrs

defListVar :: Name -> [Text] -> DecQ
defListVar v es = valD var body []
   where var  = varP v
         body = normalB . listE $ map (conE . mkNameT) es


{-

$helpers

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

fieldSetter :: AdminInterface -> [Text] -> AdminInterface
fieldSetter ai line
   | null line = ai
   | otherwise = case field of
                      "inline"   -> ai { inline   = head args              }
                      "list"     -> ai { selectionPageAttrs = Just argCons
                                       , derivedAttrs = derivedAttrs ai
                                                        `union` newAttrs
                                       }
                      "plural"   -> ai { plural   = Just $ T.unwords args   }
                      "singular" -> ai { singular = Just $ T.unwords args   }
                      "show"     -> ai { readPageAttrs = Just argCons
                                       , derivedAttrs = derivedAttrs ai
                                                        `union` newAttrs
                                       }
                      "title"    -> setTitle
   where field       = head line
         args        = tail line
         en          = name ai
         argCons     = map (constructor en) args
         newAttrs    = map (constructor en) $ filter isDerived args
         setTitle    = ai { titles = M.insert (constructor en (head args))
                                              (T.unwords $ tail args)
                                              $ titles ai
                          }

procAdminSection :: EntityDef -> AdminInterface
procAdminSection ed = foldl fieldSetter (defaultInterface ed) adminLines
    where adminLines = fromMaybe [] $ M.lookup "Admin" $ entityExtra ed


textFun :: Name -> Text -> DecQ
textFun f t = funD f [rhs []]
        where rhs = clause [wildP] $ normalB $ textL t


-- constructorE   :: Text -> Text -> ExpQ
-- constructorE e attr = conE $ mkNameT $ constructor e attr
constructorP   :: Text -> Text -> PatQ
constructorP e attr = conP (mkNameT $ constructor e attr) []



fieldName :: Text -> Text -> Text
fieldName en fn = unCapitalise $ camelCaseUnwords [en, fn]

entityName :: EntityDef -> Text
entityName = unHaskellName . entityHaskell

