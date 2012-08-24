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
       -- * Customisation and i18n.
       -- $i18n
         mkAdminInstances
       , deriveInlineDisplay
       , deriveAttributeDisplay
       , deriveAdministrable
       ) where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative

import System.FilePath
import System.Directory
import Data.Maybe
import Language.Haskell.TH
import Database.Persist.EntityDef
import Yesod

import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.Message
import Yesod.Admin.Helpers.Text
import Yesod.Admin.TH.Helpers
import Yesod.Admin.TH.Entity.AdminInterface
import Yesod.Admin.TH.Error

type Text = T.Text

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
           aCons    = attributeConsE en $ fromJust  $ inline ai
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
           mkClause at = clause [attributeConsP en at] body []
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
     where cons = attributeCons en <$> attrs ai
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
              $ map (attributeCons en)
              $ dbAttrs ai
  where en = name ai

defSelectionAttrs :: AdminInterface -> Maybe DecQ
defReadAttrs      :: AdminInterface -> Maybe DecQ

defSelectionAttrs ai = fmap genCode $ list ai
    where en         = name ai
          genCode    = defAttrListVar 'selectionPageAttributes en
                     . map withoutSortOpt

defReadAttrs ai   = fmap genCode $ readPage ai
    where en      = name ai
          genCode = defAttrListVar 'readPageAttributes en

defAttrListVar :: Name -> Text -> [Text] -> DecQ
defAttrListVar var en = defListVar var
                      . map (attributeCons en)

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




-- | Creates the attribute constructor pattern.
attributeConsP  :: Text -> Text -> PatQ
attributeConsP e attr = conP (mkNameT $ attributeCons e attr) []

-- | Create the attribute constructor expression
attributeConsE  :: Text -> Text -> ExpQ
attributeConsE e attr = conE $ mkNameT $ attributeCons e attr

-- | Create the action consructor pattern
actionConsP  :: Text -> Text -> PatQ
actionConsP e attr = conP (mkNameT $ actionCons e attr) []




attrs      :: AdminInterface -> [Text]
attrs ai   = dbAttrs ai ++ derivedAttrs ai


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
toAttributePatDef entity (at, txt) = (attributeConsP entity at, textL txt)

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

actionTransPath base ai = base </> en </> "action"
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

-- | Check action translations.
checkActionTrans :: AdminInterface
                 -> LangTrans
                 -> Either String LangTrans
checkActionTrans ai trans
                 | not $ null err = Left err
                 | otherwise      = Right trans
  where err     = errTrans (name ai) "Action" actions trans
        actions = fromJust $ action ai


-- | Check attribute translations.
checkAttributeTrans :: AdminInterface
                    -> LangTrans
                    -> Either String LangTrans
checkAttributeTrans ai trans
                    | not $ null err = Left err
                    | otherwise      = Right trans
  where err      = errTrans (name ai) "Attributes" (attrs ai) trans


-- | Check error in translations. It looks for missing names and
-- unknown names.
errTrans :: Text      -- ^ Entity name
         -> Text      -- ^ Type name Action/Attribute
         -> [Text]    -- ^ Names
         -> LangTrans -- ^ The translation
         -> String

errTrans en ty allNames (lang,mdefs)
  = unlines $ combineErrs (T.unpack tag)
                          (T.unpack <$>errs)
  where defNames = snd <$> mdefs
        unknown  = mkErrs "unknown names " $ defNames \\ allNames
        missing  = mkErrs "missing definitions of " $ allNames \\ defNames
        tag         = T.intercalate ":" [en, ty, lang]
        errs        = filter (not . T.null) [missing, unknown]
        mkErrs t ts = if null ts then ""
                         else t `T.append` T.intercalate ", " ts
