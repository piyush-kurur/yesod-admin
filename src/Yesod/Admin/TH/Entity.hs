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
       -- $adminSection
       -- ** Allowed fields.
       -- $adminFields

       -- * Attributes
       -- $attributeName
       -- ** Attribute constructors.
       -- $attributeConstructors
       -- * Admin Actions.
       -- $actionNames
       -- ** Action constructors.
       -- $actionConstructors

         AdminInterface(..)
       , mkAdminInstances
       , entityDefToInterface
       , deriveInlineDisplay
       , deriveAttributeDisplay
       , deriveAdministrable
       ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Data.Either
import Language.Haskell.TH
import Database.Persist.EntityDef
import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Helpers.Text
import Yesod.Admin.TH.Helpers
import Yesod.Admin.Class

type Text = T.Text
type Map = M.Map

-- $adminSection
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

-- $adminFields
-- [@action@] A list of allowed admin actions. Should occur at most
--    once. The words in the the list should contain either (1) the
--    word "delete" (delete) (2) a word starting with a small case
--    letter (update action) or (3) a word starting with a upper case
--    letter (a custom action). The textual representation is obtained
--    by taking the un-camelcased version of the name.  E.g.  a line
--    of the form @action delete confirmRegistration Bar@ means that
--    the object supports delete, an update action titled "Confirm
--    registration" give by the variable confirmRegistration and a
--    custom action @Bar@ given by the variable @bar@. Here bar shold
--    have the type $Key b v -> b m v$
--
-- [@inline@] Attribute used in the inline display of the
--    object. Should occur at most once in the admin section. There
--    should be a single parameter which is the name of the attribute
--    (See the convention on attribute naming). The default value is
--    the first field in the entity definition.
--
-- [@list@] A list of attributes used in the selection list display of
--    the objects. Should occur at most once in the admin section. The
--    parameter is the list of attribute and the ordering of the
--    attribute should be as required in the selection listing. Each
--    /database attribute/ (refer attribute naming converntion) is
--    optionally prefixed by either a + or a - to indicate whether the
--    selection should sort in increasing or decreasing order with
--    respect to that attribute respectively. Default value is the
--    single attribute that matches the inline display of the object.
--
-- [@show@] The list of attributes that are shown on the read page of
--    the object. This defaults to all the database attributes.
--

-- | This datatype controls the admin site generate via the template
-- haskell functions of this module. Having defined this type you can
-- use either either `mkYesodAdmin` or `mkEntityAdmin` (if you want to
-- tweak the access controls).

data AdminInterface
     = AdminInterface { name      :: Text
                      , action    :: Maybe [Text]
                      , inline    :: Maybe Text
                      , list      :: Maybe [Text]
                      , readPage  :: Maybe [Text]
                      , dbAttrs   :: [Text]
                      , derivedAttrs :: [Text]
                      } deriving Show

defaultInterface :: EntityDef -> AdminInterface
defaultInterface ed
   = AdminInterface { name     = unHaskellName $ entityHaskell ed
                    , action   = Nothing
                    , inline   = Nothing
                    , list     = Nothing
                    , readPage = Nothing
                    , dbAttrs  = das
                    , derivedAttrs = []
                    }
   where das = map (unHaskellName . fieldHaskell) $ entityFields ed


entityDefToInterface :: EntityDef -> Either String AdminInterface
entityDefToInterface ed = do ai    <- setFields
                             aichk <- chk ai
                             return $ aichk {derivedAttrs = derAttrs aichk }
   where adminLines     = fromMaybe [] $ M.lookup "Admin" $ entityExtra ed
         startAI        = defaultInterface ed
         setFields      = foldl fld (Right $ startAI) adminLines
         en             = T.unpack $ name startAI
         fld eai (x:xs) = fieldSet en eai x xs
         fld eai []     = eai
         derAttrs ai    = filter isDerived (lstAttrs ai ++ rpAttrs ai)
         lstAttrs ai    = map unSort $ fromMaybe [] $ list ai
         rpAttrs  ai    = fromMaybe [] $ readPage ai
         chk ai | null errs = Right ai
                | otherwise = Left  $ unlines errs
                where errs = checkAdminFields ai


fieldSet :: String
         -> Either String AdminInterface
         -> Text
         -> [Text]
         -> Either String AdminInterface

fieldSet en eai "action" ts  = setAction   en eai ts
fieldSet en eai "list"   ts  = setList     en eai ts
fieldSet en eai "show"   ts  = setReadPage en eai ts
fieldSet en _   "inline" []  = Left $ errMsg [ en
                                             , "inline"
                                             , " empty definition"
                                             ]
fieldSet en eai "inline" [t] = setInline en eai t
fieldSet en _ "inline"   _   = Left $ errMsg [ en
                                             , "inline"
                                             , " too many args"
                                             ]
fieldSet en _   f        _   = Left $ errMsg [ en
                                             , T.unpack f
                                             , " unknown field"
                                             ]

errMsg :: [String] -> String
errMsg = intercalate ":"

-- | This TH combinator derives the three classes @`Administrable`@
-- @`InlineDisplay`@ and @`AttributeDisplay`@ for a persistent entity.

mkAdminInstances :: [EntityDef] -> DecsQ
mkAdminInstances edefs = case err of
                              []   -> sequence $ concat code
                              errs -> fail $ unlines errs
    where mkAI ai = [ deriveAdministrable' ai
                    , deriveInlineDisplay' ai
                    , deriveAttributeDisplay' ai
                    ]
          (err,code) = partitionEithers $ map mapper edefs

          mapper     = fmap mkAI . entityDefToInterface

-- | Derive an instance of `InlineDisplay` for an entity give its
-- administrative interface.
deriveInlineDisplay  :: AdminInterface -> DecsQ

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
           attr  = fromMaybe (head $ dbAttrs ai) $ inline ai
           body = normalB $ displayRHS en attr
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
           ats = dbAttrs ai ++ derivedAttrs ai
           instBody = [ funD 'attributeDisplay $ map mkClause ats ]
           mkClause at = clause [constructorP en at] body []
                    where body = normalB $ displayRHS en at

-- | Derive an instance of @`Administrable`@ for the type @v@ given
-- the AdminInterface for @v@.

deriveAdministrable  :: AdminInterface
                     -> DecsQ
deriveAdministrable' :: AdminInterface
                     -> DecQ
deriveAdministrable  = fmap (:[]) . deriveAdministrable'

deriveAdministrable' ai = mkInstance [] ''Administrable [persistType en b]
                                     $ instBody
      where b        = varT $ mkName "b"
            en       = name ai
            instBody = [ defAttribute ai   b
                       , defAction ai b
                       , defDBAttrs   ai
                       , defDBAction  ai
                       ]
                       ++ catMaybes [ defSelectionAttrs ai
                                    , defReadAttrs      ai
                                    , defSelectionPageSort ai
                                    ]

-- | Define the attribute data type.
defAttribute :: AdminInterface      -- ^ Entity name
             -> TypeQ               -- ^ Backend
             -> DecQ

defAttribute ai = defAssocType ''Attribute cons
                               [''Eq, ''Enum, ''Bounded]
                               ai
     where cons = map (constructor en) ats
           en   = name ai
           ats  = dbAttrs ai ++ derivedAttrs ai


-- | Define the Action data type.
defAction :: AdminInterface    -- ^ Entity name
          -> TypeQ             -- ^ Backend
          -> DecQ

defAction ai   = defAssocType ''Action cons [''Enum] ai
    where cons = map (actionCons en) $ fromMaybe ["delete"] $ action ai
          en   = name ai

defAssocType :: Name           -- ^ Name of the associate.
             -> [Text]         -- ^ Constructors.
             -> [Name]         -- ^ deriving which basic classes.
             -> AdminInterface -- ^ The interface
             -> TypeQ          -- ^ The backend.
             -> DecQ


defAssocType n cons clss ai b = dataInstD (cxt []) n [persistType en b]
                                      alts clss
      where en   = name ai
            alts = [ normalC c [] | c <- map mkNameT cons ]


defDBAttrs        :: AdminInterface -> DecQ
defDBAttrs        = defListVar 'dbAttributes . dbAttrs

defSelectionAttrs :: AdminInterface -> Maybe DecQ
defReadAttrs      :: AdminInterface -> Maybe DecQ

defSelectionAttrs = fmap (defListVar 'selectionPageAttributes)
                         . fmap (map unSort) . list

defReadAttrs      = fmap (defListVar 'readPageAttributes)
                         . readPage

defSelectionPageSort :: AdminInterface
                     -> Maybe DecQ
defSelectionPageSort ai = fmap (defsps $ name ai) $ list ai

defsps :: Text -> [Text] -> DecQ
defsps en fs = valD spsVar body []
    where body   = normalB $ listE $ catMaybes $ map sortOpt fs
          spsVar = varP 'selectionPageSort
          asc  = Just . appE (conE 'Asc)
          desc = Just . appE (conE 'Desc)
          sortOpt f | T.head f   == '-' = desc $ mkEntityField en $ T.tail f
                    | T.head f   == '+' = asc  $ mkEntityField en $ T.tail f
                    | otherwise         = Nothing

mkEntityField :: Text -> Text -> ExpQ
mkEntityField e t = conE $ mkNameT $ capitalise $ camelCaseUnwords [e,t]

displayRHS :: Text
           -> Text
           -> ExpQ
displayRHS en at | isDerived at = varE $ mkNameT $ funcName at
                 | otherwise    = let fname = varE $ mkNameT
                                                   $ fieldName en at
                                      in [| inlineDisplay . $fname |]

defListVar :: Name -> [Text] -> DecQ
defListVar v es = valD var body []
   where var  = varP v
         body = normalB . listE $ map (conE . mkNameT) es


{-

Developer notes
---------------

The setters check for multiple definitions and flag an error. The
checkers check for overall consistency. The overall consistency check
that one can do is to check that every attribute name given in the
Admin section that starts with a small letter should be a
dbAttribute. This should be checked for in inline, list and show
fields.

-}



type Result = Either String AdminInterface

checkAdminFields :: AdminInterface -> [String]
checkAdminFields ai = combineErrs (T.unpack $ name ai)
                             $ concat [ inlineC
                                      , listC
                                      , rPC
                                      ]
     where inlineC = chk "inline: "
                         $ maybeToList $ inline ai
           listC   = chk "list" $ map unSort $ fromMaybe [] $ list ai
           rPC     = chk "show" $ fromMaybe [] $ readPage ai
           chk msg = combineErrs msg . checkDBAttrs ai

checkDBAttr :: AdminInterface -> Text -> [String]
checkDBAttr ai attr | isDerived attr            = []
                    | attr `elem` dbAttrs ai    = []
                    | otherwise = [ unwords [ T.unpack attr
                                            , " unknown database attribute"
                                            ]
                                  ]

checkDBAttrs :: AdminInterface -> [Text] -> [String]
checkDBAttrs ai = concatMap (checkDBAttr ai)

combineErrs :: String -> [String] -> [String]
combineErrs _   []     = []
combineErrs tag (x:xs) = [unlines (f:map (indent l) xs)]
         where f = tag ++ ": " ++ x
               l = length tag + 2
               indent len = (++) $ replicate len ' '


-- | Get rid of the sorting rule from an attribute name.
unSort :: Text -> Text
unSort t | T.head t == '+' = T.tail t
         | T.head t == '-' = T.tail t
         | otherwise      = t




setAction :: String -> Result -> [Text] -> Result
setAction en = setOnce action (\ ai x -> ai { action = Just x })
                    $ errMsg [ en, "action", " multiple definitions"]

setInline :: String -> Result -> Text -> Result
setInline en = setOnce inline (\ ai x -> ai { inline = Just x })
                    $ errMsg [ en, "inline",  " multiple definitions"]

setList :: String -> Result -> [Text] -> Result
setList en = setOnce list (\ ai x -> ai { list = Just x })
                    $ errMsg [ en, "list", " multiple definitions"]

setReadPage :: String -> Result -> [Text] -> Result
setReadPage en = setOnce readPage (\ ai x -> ai { readPage = Just x })
                    $ errMsg [ en, "show", " multiple definitions"]


setOnce :: (b -> Maybe x)
        -> (b -> x -> b)
        -> a
        -> Either a b
        -> x
        -> Either a b
setOnce g p a eb x = do b <- eb
                        maybe (Right $ p b x) (const $ Left a) $ g b


-- $attributeName
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

isDerived :: Text -> Bool
isDerived = isUpper . T.head
isDB      :: Text -> Bool
isDB      = isLower . T.head


fieldName :: Text -> Text -> Text
fieldName en fn = unCapitalise $ camelCaseUnwords [en, fn]

funcName :: Text -> Text
funcName =  unCapitalise


-- $ActionName
--
-- The action field of an persistent entity can contain a list of one
-- or more action names. An action name can be one of the following:
--
--   1. The string "delete" that captures the delete action.
--
--   2. A string that starts with an lower case letter e.g. @confirm@
--   in which case it is an update action. For such an action, there
--   should be an appropriate update defined within the scope where
--   the TH code is called. For e.g. if one uses the string @confirm@
--   in the action field of the entity @Registration@ say, then one
--   needs to define the function @registrationConfirmUpdate@ of type
--   @Update Registration@.
--
--   3. A string that starts with a upper case letter e.g. @FooBar@ in
--   which case it denotes a custom action. There should also be a
--   function with the name @fooBar@ in this case, of type @Key b v ->
--   b m v@.
--
-- The drop down menu in the selection list will list each action in
-- the same order as given in the action field.

isDelete :: Text -> Bool
isUpdate :: Text -> Bool
isCustomAction :: Text -> Bool

isDelete  = (==) "delete"
isUpdate  = isLower . T.head
isCustomAction = isUpper . T.head




{-
setOnce g p a (Right b) x = maybe (Right $ p b x) (const $ Left a) $ g b
setOnce _   _   _ leftB     _ = leftB

-}



-- $attributeConstructors
--
-- The TH code generates one constructor for each database entry plus
-- what ever constructed attributes are used in list field. The
-- constructors are the following.
--
--  1. For a database attribute the constructor is camel cased
--  concatenation of the entity name, the attribute name and the
--  string "Attribute". For example the database column @name@ of
--  entity Person will give a constructor @PersonNameAttribute@
--
--  2. For a constructed attribute corresponding to a function, the
--  constructor will be the function name with the first letter
--  capitalised. For example if the function name is @nameAndEmail@,
--  the constructor will be @NameAndEmail@
--

constructor   :: Text    -- ^ Entity name
              -> Text    -- ^ Attribute name
              -> Text

constructor en attr
   | T.null    attr    = T.empty
   | isDerived attr    = capitalise attr
   | otherwise         = capitalise $ camelCaseUnwords [ en
                                                       , attr
                                                       , "Attribute"
                                                       ]

constructorP   :: Text -> Text -> PatQ
constructorP e attr = conP (mkNameT $ constructor e attr) []

-- $actionConstructors
--
-- The TH code generates one constructor for each administrative
-- action. The constructors are the following.
--
--  1. For the delete action the constructor is camel cased
--  concatenation of the entity name, the string "Delete" and the
--  string "Action. E.g. @PersonDeleteAction@ for the entity @Person@.

--  2. For the update action it is camel cased concatenation of the
--  entity name, name of the update and the string "Update". E.g
--  @RegistrationConfirmUpdate@ for the update @confirm@ of the entity
--  @Registration@.
--
--  3. For a custom action @FooBar@, the constructor will the action
--  name itself.
--

actionCons  :: Text    -- ^ Entity name
            -> Text    -- ^ Action name
            -> Text
actionCons en act
   | T.null    act   = T.empty
   | act == "delete" = camelCaseUnwords [ en
                                           , "Delete"
                                           , "Action"
                                           ]
   | isUpdate act    = camelCaseUnwords [ en
                                        , act
                                        , "Update"
                                        ]
   | otherwise       = act

actionConsP  :: Text -> Text -> PatQ
actionConsP e attr = conP (mkNameT $ constructor e attr) []

actionRHS :: Text -> Text -> ExpQ
actionRHS en act | act == "delete" = conE 'DBDelete
                 | isUpdate act    = conE 'DBUpdate `appE` updateExp
                 | otherwise       = conE 'DBCustom `appE` customExp
    where updateExp = varE $ mkNameT $ unCapitalise $ actionCons en act
          customExp = varE $ mkNameT act

defDBAction :: AdminInterface
            -> DecQ
defDBAction ai = singleArgFunc 'dbAction
                 $ [ (actionConsP en  act, actionRHS en act) | act <- acts ]
    where acts   = fromMaybe ["delete"] $ action ai
          en     = name ai

{-
-- FIXME: Write a Quick check test to check dbAttrToFieldName
-- (constructer e x) = x where ever x starts with a lower case
-- alphabet.

dbAttrToFieldName t = unCapitalise $ camelCaseUnwords $
                      init $ tail $ unCamelCaseWords t













defObjectSingular :: AdminInterface -> Maybe DecQ
defObjectPlural   :: AdminInterface -> Maybe DecQ

defObjectSingular = fmap (textFun 'objectSingular) . singular
defObjectPlural   = fmap (textFun 'objectPlural) . plural



defAttributeTitle :: AdminInterface
                  -> DecQ
defAttributeTitle ai = singleArgFunc 'attributeTitle
                               $ [ (mkC c, textL t) | (c,t) <- cts]
    where cts   = M.toList $ titles ai
          mkC c = conP (mkNameT c) []









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

procAdminSection :: EntityDef -> AdminInterface
procAdminSection ed = foldl fieldSetter (defaultInterface ed) adminLines
    where


textFun :: Name -> Text -> DecQ
textFun f t = funD f [rhs []]
        where rhs = clause [wildP] $ normalB $ textL t


-- constructorE   :: Text -> Text -> ExpQ
-- constructorE e attr = conE $ mkNameT $ constructor e attr




entityName :: EntityDef -> Text
entityName = unHaskellName . entityHaskell


-}
