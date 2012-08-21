{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-|

Module to generate admin code for persistent entries.

-}

module Yesod.Admin.TH.Entity
       (
       -- * Admin section.
       -- $adminSection

       -- * Attributes.
       -- $Attributes

       -- * Actions.
       -- $Actions

       -- * Customisation and i18n.
       -- $i18n

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
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Control.Applicative

import System.FilePath
import System.Directory
import Data.Maybe
import Data.Either
import Language.Haskell.TH
import Database.Persist.EntityDef
import Yesod

import Yesod.Admin.Types
import Yesod.Admin.Helpers.Text
import Yesod.Admin.TH.Helpers
import Yesod.Admin.Class
import Yesod.Admin.Message

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
--    The default value is the first field, i.e. @name@ in the above
--    exampe, in the entity definition.
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


getSortOpt :: Text    -- ^ Entity name
           -> Text    -- ^ Field name
           -> Maybe ExpQ
getSortOpt en f | T.head f == '+' = Just $ asc  $ mkEntityField en $ T.tail f
                | T.head f == '-' = Just $ desc $ mkEntityField en $ T.tail f
                | otherwise       = Nothing
   where asc  e = [|Asc  $e |]
         desc e = [|Desc $e |]

-- | Once the admin section is parsed, this function sets the default
-- values of all unsef fields.
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
           lst      = map unSort    $ fromJust $ list ai
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
       where attr = unSort lattr
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

{- Error message generation -}

errMsg :: [String] -> String
errMsg = intercalate ":"

errMultiple :: String -> String -> String
errEmpty    :: String -> String -> String
errTooMany  :: String -> String -> String

errMultiple en f = errMsg [en, f, "multiple definition"]
errEmpty    en f = errMsg [en, f, "empty definition"]
errTooMany  en f = errMsg [en, f, "too many args"]

combineErrs :: String -> [String] -> [String]
combineErrs _   []     = []
combineErrs tag (x:xs) = [unlines (f:map (indent l) xs)]
         where f = tag ++ ": " ++ x
               l = length tag + 2
               indent len = (++) $ replicate len ' '

{- End of error message generation -}



-- | To use the crud and selection subsites of an entity, we need to
-- derive a few instances for an entity. This combinator derives a
-- default instance for all the entities specified in its
-- argument. Typically you would want to use this with the persist
-- quasi-quoter.
mkAdminInstances :: [EntityDef] -> DecsQ
mkAdminInstances edefs =  mkAdminInstances' edefs


-- | This combinator is similar to @`mkAdminInstances`@ but does not
-- derive the @`RenderMessage`@ instance for attributes and actions of
-- the entity. This is useful if you want to support i18n or want to
-- overried the defaults choosen.
mkAdminInstances' :: [EntityDef] -> DecsQ
mkAdminInstances' edefs = do aE <- defEns
                             insts <- withEntityDefs genCode edefs
                             return (aE:insts)
    where genCode ai = [ deriveAdministrable' ai
                       , deriveInlineDisplay' ai
                       , deriveAttributeDisplay' ai
                       ]
          entities   = map (T.unpack . unHaskellName . entityHaskell) edefs
          body       = normalB $ listE $ map stringE entities
          defEns     = valD (varP $ mkName "adminEntities") body []



-- | Derive an instance of `InlineDisplay` for an entity give its
-- administrative interface.
deriveInlineDisplay  :: AdminInterface -> DecsQ

-- | Similar to `deriveInlineDisplay` but does not wrap the
-- declaration inside a list. Not very useful in the wild as splicing
-- expects `DecsQ` instead `DecQ` but useful in defining other
-- template haskell function. Currently not exported.
deriveInlineDisplay' :: AdminInterface
                     -> DecQ
deriveInlineDisplay  = fmap (:[]) . deriveInlineDisplay'
deriveInlineDisplay' ai =
               mkInstance [persistStoreP b m]
                          ''InlineDisplay [b, m, persistType en b]
                          instBody
     where b        = varT $ mkName "b"
           m        = varT $ mkName "m"
           en       = name ai
           aCons    = conE $ mkNameT $ attrCons en $ fromJust  $ inline ai
           body     = normalB [| attributeDisplay $aCons |]
           instBody = [valD (varP 'inlineDisplay) body []]


-- | Derive an instance of `AttributeDisplay` for an entity give its
-- administrative interface.
deriveAttributeDisplay :: AdminInterface
                       -> DecsQ
deriveAttributeDisplay = fmap (:[]) . deriveAttributeDisplay'

-- | Same as `deriveAttributeDisplay` but does not wrap the
-- declaration inside a list. Not very useful in the wild as splicing
-- expects `DecsQ` instead `DecQ` but useful in defining other
-- template haskell function. Currently not exported.
deriveAttributeDisplay' :: AdminInterface
                        -> DecQ
deriveAttributeDisplay' ai
            = mkInstance [persistStoreP b m]
                    ''AttributeDisplay [b, m, persistType en b]
                    [attrDisp]
     where b  = varT $ mkName "b"
           m  = varT $ mkName "m"
           en = name ai
           attrDisp = funD 'attributeDisplay $ map mkClause $ attrs ai
           mkClause at = clause [attrConsP en at] body []
                    where body = normalB $ rhs at
           rhs at | isDerived at = varE $ mkNameT $ attributeFunctionName en at
                  | otherwise    = [|inlineDisplay . $fname|]
                     where fname = varE $ mkNameT $ attributeFieldName en at

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



-- $titles
--
-- Actions and Attributes should be instances of
-- @`RenderMessage`@ and the default instances are derived by the TH code. The
-- default rule .This allows us to i18nize the admin
-- site.
--

defaultMessage :: Text -> ExpQ
defaultMessage = textL . capitalise . unCamelCase



-- | Define the attribute data type.
defAttribute :: AdminInterface      -- ^ Entity name
             -> TypeQ               -- ^ Backend
             -> DecQ

defAttribute ai = defAssocType ''Attribute cons
                               [''Eq, ''Enum, ''Bounded]
                               ai
     where cons = map (attrCons en) $ attrs ai
           en   = name ai


-- | Define the Action data type.
defAction :: AdminInterface    -- ^ Entity name
          -> TypeQ             -- ^ Backend
          -> DecQ

defAction ai   = defAssocType ''Action cons [''Enum, ''Eq, ''Bounded] ai
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




defDBAttrs    :: AdminInterface -> DecQ
defDBAttrs ai = defListVar 'dbAttributes
              $ map (attrCons en)
              $ dbAttrs ai
  where en = name ai

defSelectionAttrs :: AdminInterface -> Maybe DecQ
defReadAttrs      :: AdminInterface -> Maybe DecQ

defSelectionAttrs ai = fmap genCode $ list ai
    where en         = name ai
          genCode    = defAttrListVar 'selectionPageAttributes en
                     . map unSort

defReadAttrs ai   = fmap genCode $ readPage ai
    where en      = name ai
          genCode = defAttrListVar 'readPageAttributes en

defAttrListVar :: Name -> Text -> [Text] -> DecQ
defAttrListVar var en = defListVar var
                      . map (attrCons en)

defSelectionPageSort :: AdminInterface
                     -> Maybe DecQ
defSelectionPageSort ai = fmap decl $ list ai
    where en       = name ai
          body     = normalB . listE . catMaybes . map (getSortOpt en)
          selpPat  = varP 'selectionPageSort
          decl lst = valD selpPat (body lst) []



defListVar :: Name -> [Text] -> DecQ
defListVar v es = valD var body []
   where var  = varP v
         body = normalB . listE $ map (conE . mkNameT)  es



-- | Get rid of the sorting rule from an attribute name.
unSort :: Text -> Text
unSort t | T.head t == '+' = T.tail t
         | T.head t == '-' = T.tail t
         | otherwise      = t







actions :: AdminInterface
        -> [Text]
actions = fromMaybe ["delete"] . action

defDBAction :: AdminInterface
            -> DecQ
defDBAction ai = singleArgFunc 'dbAction
                 $ [ (actionConsP en act, rhs act) | act <- acts ]
    where acts    = fromMaybe ["delete"] $ action ai
          en      = name ai
          rhs act | act == "delete" = conE 'DBDelete
                  | isUpdate act    = conE 'DBUpdate `appE` actFunc act
                  | otherwise       = conE 'DBCustom `appE` actFunc act
          actFunc act = varE $ mkNameT $ actionFunctionName en act


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


-- $Attributes
--
-- Attributes of an entity is used in selection pages, display pages
-- and in the inline display of an entity. All the fields of the
-- persistent entity are themselves attributes. These are the database
-- attributes of the entity as they are stored in the database backend
-- in some form. Besides one can define other /derived attributes/,
-- i.e.  attributes of the entity that are not stored in the database
-- but can be computed. In the fields inline, list and show of the
-- admin section the following convention is used:
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


attrCons   :: Text    -- ^ Entity name
           -> Text    -- ^ Attribute name
           -> Text
attrCons = constructor "Attribute"

attrConsP  :: Text -> Text -> PatQ
attrConsP e attr = conP (mkNameT $ attrCons e attr) []




attributeFieldName :: Text -> Text -> Text
attributeFieldName en fn = unCapitalise $ camelCaseUnwords [en, fn]

attributeFunctionName :: Text -> Text -> Text
attributeFunctionName en =  unCapitalise .  attrCons en


attrs      :: AdminInterface -> [Text]
attrs ai   = dbAttrs ai ++ derivedAttrs ai

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

actionFunctionName  :: Text  -- ^ Entity name
                    -> Text  -- ^ Action name
                    -> Text
actionFunctionName en = unCapitalise  . actionCons en


actionConsP  :: Text -> Text -> PatQ
actionConsP e attr = conP (mkNameT $ actionCons e attr) []



isDelete :: Text       -- ^ action name
         -> Bool       -- ^ Is the action name a delete action.
isDelete  = (==) "delete"


isUpdate :: Text   -- ^ action name
         -> Bool   -- ^ Is the action name an update action
isUpdate  act = isLower (T.head act) && not (isDelete act)

isCustomAction :: Text  -- ^ action name
               -> Bool  -- ^ Is the action name a custom action
isCustomAction = isUpper . T.head


{- The I18N code:

We start by defining some local type aliases to improve code
readability. These are not meant to be exported.

-}

type MesgDef      = (Text, Text)         -- ^ message constructor and
                                         -- its rendering text
type MesgPatDef   = (PatQ, ExpQ)         -- ^ message constructor
                                         -- pattern and its rendering
                                         -- expression
type LangTrans    = (Lang, [MesgDef])    -- ^ language translation
type LangPatTrans = (ExpQ, [MesgPatDef]) -- ^ language translation
                                         -- pattern and its message
                                         -- definition pattern

-- | Given a function to generate the constructor name and a message
-- definition returns the corresponding definition in TH form.

toMesgPatDef :: (Text -> PatQ)  -- ^ constructor generator
             -> MesgDef         -- ^ The message definition
             -> MesgPatDef
toMesgPatDef consGen (consName, txt) = (consGen consName, textL txt)

-- | Given a function to generate the constructor name and a language translation
-- returns the
toLangPatTrans :: (Text -> PatQ) -- ^ The constructor generator
               -> LangTrans      -- ^ The language translation
               -> LangPatTrans

toLangPatTrans consGen (lang, mdefs)
  = (textL lang, toMesgPatDef consGen <$> mdefs)

-- | Generate an attribute message definition.
toAttributePatDef :: Text       -- ^ Entity name
                  -> MesgDef    -- ^ The message definition
                  -> MesgPatDef
toAttributePatDef entity (at, txt) = (attrConsP entity at, textL txt)

-- | Generate an action message definition.
toActionPatDef :: Text        -- ^ Entity name
               -> MesgDef     -- ^ The message definition
               -> MesgPatDef
toActionPatDef entity (act, txt) = (actionConsP entity act, textL txt)




-- | The workhorse function for i18n.
mkMessage :: TypeQ          -- ^ For which type
          -> [LangPatTrans] -- ^ The translations, empty for default
                            -- instance.
          -> [MesgPatDef]   -- ^ The default definitions
          -> DecQ
mkMessage msgTyp trans defs =
   mkInstance [] ''RenderMessage [master, msgTyp]
                 [funD 'renderMessage cls]
   where cls = if null trans then [defaultClause defs]
                  else [translateClause trans, defaultClause defs]
         master = varT $ mkName "master"



-- | Create a default clause out of the message definition.
defaultClause :: [MesgPatDef] -> ClauseQ
defaultClause mdef = do msgV <- newName "msg"
                        clause [wildP, wildP, varP msgV]
                               (normalB $ mkCase msgV mdef)
                               []

translateClause :: [LangPatTrans] -> ClauseQ
translateClause ldefs = do master <- newName "master"
                           l      <- newName "l"
                           ls     <- newName "ls"
                           msg    <- newName "msg"
                           trans master l ls msg
  where trans master l ls msg = clause [ masterP
                                       , lstPat
                                       , msgP
                                       ]
                                       (guardedB guards)
                                       []
              where masterP = varP master
                    lstPat  = conP '(:) [varP l, varP ls]
                    msgP    = varP msg
                    guards  = map (langGuard l msg) ldefs
                              ++ [ langRest master ls msg ]



-- | Create a guard with a given language expression.
langGuard :: Name          -- ^ lang variable name
          -> Name          -- ^ mesage variable
          -> LangPatTrans  -- ^ the language definition
          -> Q (Guard, Exp)
langGuard l msg (lval,mpats) = normalGE [| $lexp == $lval |]
                                        $ mkCase msg mpats
  where lexp = varE l

-- | Check rendering with the rest of the languages
langRest :: Name -- ^ master var
         -> Name -- ^ rest of the languages
         -> Name -- ^ message variable
         -> Q (Guard, Exp)
langRest master ls msg = normalGE [| otherwise |]
                                  [| renderMessage $masterE $lsE $msgE |]
  where masterE = varE master
        lsE     = varE ls
        msgE    = varE msg


-- | Creates a case expression which generates the actual rendering.
mkCase :: Name          -- ^ message variable
       -> [MesgPatDef]  -- ^ message definition
       -> ExpQ
mkCase v = caseE (varE v) . map mkCl
    where mkCl (p,b) = match p (normalB b) []



-- | Get the language name from the file name.
langName :: FilePath -> Lang
langName = T.pack . dropExtension . takeFileName

-- | Check if the file is a valid message file.
isMessageFile :: FilePath -> Bool
isMessageFile f = takeExtension f == (extSeparator:"msg")

-- | Get all the message files.
getMessageFiles :: FilePath -> IO [FilePath]
getMessageFiles dir = filter isMessageFile <$> getDirectoryContents dir


-- $i18n
--
-- For an entity @v@ the admin site requires the types @'Attribute'
-- v@, @'Action' v@ and @Collective v@ to be instances of
-- @'RenderMessage'@. Even if you plan to support only one language,
-- you might not be happy by the default choices for each of these
-- stuff. In either of these cases you need to create translation
-- files for these data types.
--
-- Defining the render message instances involves setting up of the
-- necessary translation files in an appropriate directory structure.
-- The directory structure is as follows:
--
--   1. On the topmost level there is an optional directory per
--      entity. Create a directory for which you want to configure
--      these message types.
--
--   2. For each entity there are three (optional) subdirectories
--
--      i.  action: This contains the translation files for the associated type
--          @'Action'@ of the entity
--
--      ii. attribute: This contains the translation files for the
--          associated type @'Attribute'@ of the entity
--
--      iii. collective: Message files for transaltion of the
--           @'Collective'@ datatype
--
-- The translation files for attributes consists of lines of attribute
-- name and attribute title separated by a ':' (colon). Attribute
-- names follow the naming convention what we have defined before,
-- i.e. names starting with lower case denote database attributes and
-- those that starts with upper case denote derived attributes. As an
-- example, the fr.msg file that defines the two attributes, first a
-- database attribute @firstName@ and second a derived attribute
-- @NameAndEmail@ would be:
--
-- > firstName: Prénom
-- > NameAndEmail: Nom et email
--
-- The translation files for actions are similar except that we use
-- action names instead of attribute name.
--
-- The translation files for @Collective@ follows the convention
-- followed by the function @mkMessageFor@.

-- | Path to the translation file for actions.
actionTransPath :: FilePath       -- ^ Base admin directory
                -> AdminInterface -- ^ The admin interfaces for the entity
                -> FilePath

actiontransPath base ai = base </> en </> "action"
   where en = T.unpack $ name ai

-- | Path to the translation file for actions.
attributeTransPath :: FilePath
                   -> AdminInterface
                   -> FilePath
attributeTransPath base ai = base </> en </> "attribute"
   where en = T.unpack $ name ai

parseMesgDir :: FilePath  -- ^ The directory where the transation
                          -- files are
             -> IO [LangTrans]
parseMesgDir dir = getMessageFiles dir >>= sequence . map (parseMesg dir)


parseMesg :: FilePath -> FilePath -> IO LangTrans
parseMesg dir f   = do cont  <- TIO.readFile (dir </> f)
                       return (langName f, parse cont)
    where fieldValue x = (T.strip u, T.strip $ T.tail v)
                 where (u,v) = T.breakOn ":" x
          parse cont   = [ fieldValue x | x <-  T.lines cont
                                        , not $ T.null $ T.strip x
                         ]
