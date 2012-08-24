{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Admin.TH.Entity.AdminInterface
       (
       -- * The admin section
       -- $adminSection

         AdminInterface(..)
       , attributes
       , entityDefToInterface
       , withEntityDefs
       -- * Attributes
       -- $Attributes
       , isDB
       , isDerived
       , attributeCons
       , attributeConsP
       , attributeConsE
       , attributeFieldName
       , attributeFunctionName
       , withoutSortOpt
       , getSortOpt
       -- * Actions
       -- $Actions
       , isDelete
       , isUpdate
       , isCustomAction
       , actionFunctionName
       , actionCons
       , actionConsP
       ) where


import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Database.Persist.EntityDef
import Database.Persist.Query
import Language.Haskell.TH
import qualified Data.Text as T
import qualified Data.Map as M

import Yesod.Admin.TH.Error
import Yesod.Admin.TH.Helpers
import Yesod.Admin.Helpers.Text

type Text = T.Text

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

-- | Get the list of attribute.
attributes :: AdminInterface -> [Text]
attributes ai = dbAttrs ai ++ derivedAttrs ai

-- | Creates the starting admin interfaces.
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

-- | Parse the administrative interface from the entity definition.
entityDefToInterface :: EntityDef -> Either String AdminInterface
entityDefToInterface ed = do ai <- setFields
                             chk $ setDefaults ai

   where adminLines     = fromMaybe [] $ M.lookup "Admin" $ entityExtra ed
         startAI        = defaultInterface ed
         setFields      = foldl fld (Right startAI) adminLines
         en             = T.unpack $ name startAI
         fld eai (x:xs) = fieldSet en eai x xs
         fld eai []     = eai
         chk ai | null errs = Right ai
                | otherwise = Left  $ unlines errs
                where errs = checkAdminFields ai

-- | A convenient function to run th code on a list of entity defs.
withEntityDefs :: (AdminInterface -> [DecQ])
               -> [EntityDef]
               -> DecsQ
withEntityDefs genCode edefs = case err of
                                    [] -> sequence $ concat code
                                    errs -> fail $ unlines errs
    where (err,code) = partitionEithers $ map mapper edefs
          mapper     = fmap genCode . entityDefToInterface

-- $adminSection
--
-- The administrative interface of an entity is configured through the
-- (optional) section named \"Admin\". If this section is absent a
-- sane set of defaults are used. The admin section consists of lines
-- where the first entry is a field and the rest is its arguments. One
-- can configure how entities appear on admin pages, what
-- administrative mass action can be applied on them etc. As an
-- example, consider the definition given below. We have defined the
-- /fields/ @inline@ of the entity Person
--
-- >      [persist|
-- >                Person
-- >                        name    Text
-- >                        email   Text
-- >                        age     Text
-- >                        address Text
-- >                        Admin
-- >                            inline NameAndEmail
-- >                            list   +name email -age
-- >
-- >     |]
--
--
-- The allowed fields in the admin section are the following
--
-- [@action@] A list of allowed admin actions. By default only the
--    delete action is defined.
--
-- [@inline@] Attribute used in the inline display of the
--    object. Should occur at most once in the admin section. There
--    should be a single parameter which is the name of the attribute.
--    The default value is the first field in the entity definition:
--    i.e. @name@ in the above exampe.
--
-- [@list@] A list of attributes used in to display the object in the
--    selection list. The /database attribute/ can optionally be
--    prefixed by either a + or a - to indicate whether the selection
--    should sort in increasing or decreasing order with respect to
--    that attribute respectively. Default value is the single
--    attribute that matches the inline display of the object.
--
-- [@show@] The list of attributes that are shown on the read page of
--    the object. This defaults to all the database attributes.



-- | Once the admin section is parsed, this function sets the default
-- values of all unset fields.
setDefaults :: AdminInterface -> AdminInterface
setDefaults ai = ai { action        = Just act
                    , inline        = Just inl
                    , list          = Just lst
                    , readPage      = Just rp
                    , derivedAttrs  = nub derAttrs
                    }
  where act      = fromMaybe ["delete"] $ action ai
        inl      = fromMaybe (head $ dbAttrs ai) $ inline ai
        lst      = fromMaybe [inl] $ list ai
        rp       = fromMaybe (dbAttrs ai) $ readPage ai
        derAttrs = filter isDerived $ [inl] ++ lst ++ rp


getSortOpt :: Text    -- ^ Entity name
           -> Text    -- ^ Field name
           -> Maybe ExpQ
getSortOpt en f | T.head f == '+' = Just $ asc  $ mkEntityField en $ T.tail f
                | T.head f == '-' = Just $ desc $ mkEntityField en $ T.tail f
                | otherwise       = Nothing
   where asc  e = [|Asc  $e |]
         desc e = [|Desc $e |]

-- | Get rid of the sorting rule from an attribute name.
withoutSortOpt :: Text -> Text
withoutSortOpt t | T.head t == '+' = T.tail t
                 | T.head t == '-' = T.tail t
                 | otherwise      = t




-- $Attributes
--
-- Attributes of an entity is used in selection pages, display pages
-- and in the inline display of an entity. All the fields of the
-- persistent entity are themselves attributes. These are the database
-- attributes of the entity as they are stored in the database backend
-- in some form. Besides one can define other /derived attributes/,
-- i.e.  attributes of the entity that are not stored in the database
-- but are computed from the entity. In the fields inline, list and
-- show of the admin section the following convention is used:
--
--   1. A string that starts with an lower case letter, e.g. @name@,
--      is a database attribute. If the entity is @Person@ the
--      constructor of @'Attribute' Person@ associated with the
--      database attribute @name@ would be @PersonNameAttribute@.
--
--   2. A string that starts with a upper case letter
--      e.g. @NameAndEmail@ is a derived attribute. For the entity
--      @Person@, declaring a derived attribute @NameAndEmail@ should
--      be accompanied by the definition of the variable
--      @personNameAndEmailAttribute@ with type @Person -> b m 'Text'@
--      somewhere in the scope. The corresponding constructor is
--      @PersonNameAndEmailAttribute@.
--
-- Attribute are displayed as column titles in selection list and in
-- fields on the display page. To support i18n, they have to be
-- instances of @'RenderMessage'@. The TH functions generate a default
-- instance in which the rendering of the attribute is its
-- uncamelcased and name. For e.g. name becomes \"Name\" and
-- NameAndEmail becomes \"Name and Email\". If you want to customise
-- it or support multiple languages see the i18n section for details.


isDerived :: Text -> Bool
isDerived = isUpper . T.head
isDB      :: Text -> Bool
isDB      = isLower . T.head

-- | This is a refactor of both attrCons and actionCons. The
-- expression @constructor e m s@ is the camelcase concatination of e,
-- m and s if m is lower case and just m if it is upper case. This is
-- a design pattern in constructors of associated types in
-- Administrable.
constructor :: Text     -- ^ Suffix
            -> Text     -- ^ entity name
            -> Text     -- ^ middle name
            -> Text
constructor s e m = camelCaseUnwords [e,m,s]


-- | Computes the name of the attribute constructor.
attributeCons   :: Text    -- ^ Entity name
                -> Text    -- ^ Attribute name
                -> Text
attributeCons = constructor "Attribute"


-- | Creates the attribute constructor pattern.
attributeConsP  :: Text -> Text -> PatQ
attributeConsP e attr = conP (mkNameT $ attributeCons e attr) []

-- | Create the attribute constructor expression
attributeConsE  :: Text -> Text -> ExpQ
attributeConsE e attr = conE $ mkNameT $ attributeCons e attr

-- | Creates the attribute field name.
attributeFieldName :: Text -> Text -> Text
attributeFieldName en fn = unCapitalise $ camelCaseUnwords [en, fn]

-- | The name of function associated with a derived attribute
attributeFunctionName :: Text -> Text -> Text
attributeFunctionName en =  unCapitalise .  attributeCons en



-- $Actions
--
-- Actions are mass db actions that can be applied from the selection
-- page. Be default an entity supports only the delete action. The
-- allowed actions on an entity can be configured by setting the
-- @action@ field in the admin section of the entity. The field
-- arguments is a list of words which can be either of the following:
--
--    1. The word \"delete\" for the delete action. If the entity is
--       @Person@ then corresponding constructor of @`Action` Person@
--       will be @PersonDeleteAction@.
--
--    2. Any word starting with a small case letter. This denotes an
--       update action. For the entity @Person@, declaring an update
--       action named @foo@ should be accompanied by the definition of
--       the variable @personFooUpdate@ somewhere in the scope of the
--       where the TH code is called. The corresponsing constructor is
--       @PersonFooUpdate@.
--
--    3. Any word starting with a upper case letter (a custom action).
--       For the @Person@ entity, declaring a custom action \"@Bar@\",
--       should be accompanied by the definition of the variable
--       @personBarAction@ (of type @'Key' b Person -> b m ()@)
--       somewhere in the scope of the module. The corresponsing
--       constructor is @PersonBarAction@.
--
-- The drop down menu in the selection list will list each action in
-- the same order as given in the action field.
--
-- Actions are displayed in the drop down list on the selection
-- sites. Like Attributes they should be an instance of
-- @`RenderMessage`@ and the default instance generated follows the
-- same convention as attributes. See the i18n section for
-- customisation.



actionCons  :: Text    -- ^ Entity name
            -> Text    -- ^ Action name
            -> Text
actionCons en act
   | isUpdate act = constructor "Update" en act
   | otherwise    = constructor "Action"  en act

-- | Create the action consructor pattern
actionConsP  :: Text -> Text -> PatQ
actionConsP e attr = conP (mkNameT $ actionCons e attr) []

-- | The function name associated with an action
actionFunctionName  :: Text  -- ^ Entity name
                    -> Text  -- ^ Action name
                    -> Text
actionFunctionName en = unCapitalise  . actionCons en




isDelete :: Text       -- ^ action name
         -> Bool       -- ^ Is the action name a delete action.
isDelete  = (==) "delete"


isUpdate :: Text   -- ^ action name
         -> Bool   -- ^ Is the action name an update action
isUpdate  act = isLower (T.head act) && not (isDelete act)

isCustomAction :: Text  -- ^ action name
               -> Bool  -- ^ Is the action name a custom action
isCustomAction = isUpper . T.head


{- Developer notes: Code that controls setting of fields. -}

fieldSet :: String
         -> Either String AdminInterface
         -> Text
         -> [Text]
         -> Either String AdminInterface

fieldSet en eai "action" ts  = setAction   en eai ts
fieldSet en eai "list"   ts  = setList     en eai ts
fieldSet en eai "show"   ts  = setReadPage en eai ts
fieldSet en eai "inline" ts  = setInline   en eai ts
fieldSet en _   f        _   = Left $ errMsg [ en
                                             , T.unpack f
                                             , " unknown field"
                                             ]

setAction :: String -> Result -> [Text] -> Result
setAction en = setOnce en "action" action setter
        where setter ai x = ai { action = Just x }

setInline :: String -> Result -> [Text] -> Result
setInline en  _  []  = Left $ errEmpty en "inline"
setInline en eai [t] = setOnce en "inline" inline setter eai t
                      where setter ai x = ai { inline = Just x }
setInline en  _  _ = Left $ errTooMany en "inline"

setList :: String -> Result -> [Text] -> Result
setList en = setOnce en "list" list setter
     where setter ai x = ai { list = Just x }

setReadPage :: String -> Result -> [Text] -> Result
setReadPage en = setOnce en "show" readPage setter
        where setter ai x = ai { readPage = Just x }



setOnce :: String         -- ^ Entity name
        -> String         -- ^ Field name
        -> (b -> Maybe x) -- ^ The field getter
        -> (b -> x -> b)  -- ^ The field setter
        -> Either String b
        -> x
        -> Either String b
setOnce en f g p eb x = do b <- eb
                           maybe (Right $ p b x) err $ g b
         where err = const $ Left  $ errMultiple en f

{- End of field setting code -}

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
checkAdminFields ai = combineErrs en
                             $ concat [ inlineC
                                      , listC
                                      , rPC
                                      ]
     where inlineC  = chk "inline: " $ maybeToList $ inline ai
           lst      = map withoutSortOpt    $ fromJust $ list ai
           rPC      = chk "show"    $ fromJust $ readPage ai
           listC    = chk "list" lst ++ schk lst
           chk msg  = combineErrs msg . checkDBAttrs ai
           schk     = combineErrs "list" . concatMap checkSort
           en       = T.unpack $ name ai

-- | Check whether list entries with a sort specification are
-- dbEntries.
checkSort :: Text -> [String]
checkSort lattr | attr   == lattr        = []
                | isDB attr              = []
                | otherwise              = [ err ]
       where attr = withoutSortOpt lattr
             err  = unwords [ "sorting option not supported for attribute"
                            , T.unpack attr
                            ]

-- | Checks whether the given attribute is a DBAttribute.
checkDBAttr :: AdminInterface -> Text -> [String]
checkDBAttr ai attr | isDerived attr            = []
                    | attr `elem` dbAttrs ai    = []
                    | otherwise = [ unwords [ T.unpack attr
                                            , " unknown database attribute"
                                            ]
                                  ]

checkDBAttrs :: AdminInterface -> [Text] -> [String]
checkDBAttrs ai = concatMap (checkDBAttr ai)


{- End of overall check of AI -}


errMultiple :: String -> String -> String
errEmpty    :: String -> String -> String
errTooMany  :: String -> String -> String

errMultiple en f = errMsg [en, f, "multiple definition"]
errEmpty    en f = errMsg [en, f, "empty definition"]
errTooMany  en f = errMsg [en, f, "too many args"]

