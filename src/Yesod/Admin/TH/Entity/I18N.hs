{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE QuasiQuotes                  #-}

module Yesod.Admin.TH.Entity.I18N
       (
       -- * Customisation and i18n.
       -- $i18n

       -- ** Directory structure.
       -- $directoryStructure

       -- ** Translation file syntax
       -- $transFile

       ) where

import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import System.Directory
import System.FilePath
import Text.Shakespeare.I18N


import Yesod.Admin.Helpers.Text
import Yesod.Admin.TH.Helpers
import Yesod.Admin.TH.Error
import Yesod.Admin.TH.Entity.AdminInterface


type Text = T.Text

-- $i18n
--
-- For an entity @v@ the associated @'Attribute' v@, @'Action' v@ and
-- the message datatype 'Collective' has to be an instance of
-- 'RenderMessage'. The default instance for @'Attribute'@ and
-- @'Action'@ prints the name in uncamelcased form, i.e. @firstName@
-- and @nameAndEmail@ becomes @First name@ and @Name and email@
-- respectively, whatever be your language setting. In case you want
-- support multiple languages or just change the default translation
-- you have to define the necessary translation files.

-- | The default message associated with a text.
defaultMessage :: Text -> ExpQ
defaultMessage = textL . capitalise . unCamelCase


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


-- $directoryStructure
--
-- Given a base directory, the translation files of different
-- persistent entities have to be placed according to the following
-- directory structure.
--
--   1. On the topmost level, i.e. in the base directory, there is an
--      (optional) directory per entity. If the director for an entity
--      does not exist, then the rendering is governed by the default
--      rules as described before.
--
--   2. For each entity there are three (optional) subdirectories
--
--      i.  action: This contains the translation files for the
--          associated type @'Action'@ of the entity.
--
--      ii. attribute: This contains the translation files for the
--          associated type @'Attribute'@ of the entity.
--
--      iii. collective: Message files for transaltion of the
--      @'Collective'@ datatype.
--
--      The translation files are files that end with an extension
--      "msg". Again, if any of the directory is ommited then the
--      corresponding associated type have the default rendering

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



-- $transFile
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

-- | Create the translation Text from message definitions.
transText :: [MesgDef] -> Text
transText = T.unlines . map sep
     where sep (f,d) = f <> ":" <> d


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
  where err      = errTrans (name ai) "Attributes" (attributes ai) trans


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
