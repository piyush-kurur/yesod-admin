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
         
         mkDefaultTransFiles

       ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Haskell.TH
import System.Directory
import System.FilePath
import Text.Shakespeare.I18N

import Yesod.Admin.Class
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
defaultMessage :: Text -> Text
defaultMessage = capitalise . unCamelCase


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

-- | The default message definition for actions of an entity.
defaultActionMesgDef :: AdminInterface -> [MesgDef]
defaultActionMesgDef ai = [ (a, defaultMessage a) | a <- fromJust $ action ai ]

-- | The default message definition for attributes of an entity.
defaultAttributeMesgDef :: AdminInterface -> [MesgDef]
defaultAttributeMesgDef ai = [ (a, defaultMessage a) | a <- attributes ai ]

-- | Given a function to generate the constructor name and a message
-- definition, returns the corresponding definition in TH form.

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


--
-- Now the actual TH code generators. The render message instance
-- would look as follows
--
-- renderMessage master (l:ls) msg
--               | l == "en  = case msg of ...
--               | l == "fr" = case msg of ..
--               ...
--               | otherwise = renderMessage master ls msg
--
-- renderMessage _ _ msg     = case msg of ...
--
--
-- The case expression depends on the MesgDef for the given
-- language. This is the first function that we write.
--




-- | Creates a case expression which generates the actual rendering.
mkCase :: ExpQ          -- ^ message variable
       -> [MesgPatDef]  -- ^ message definition
       -> ExpQ
mkCase mesgE = caseE mesgE . map mkCl
    where mkCl (p,b) = match p (normalB b) []

--
-- The above function definition requires two kinds of clauses. One
-- for the default message definition and the other for the guarded
-- one. We first give the default clause.
--
--


-- | Create a default clause out of the message definition.
defaultClause :: [MesgPatDef] -> ClauseQ
defaultClause mdef = do msgV <- newName "msg"
                        clause [wildP, wildP, varP msgV]
                               (normalB $ mkCase (varE msgV) mdef)
                               []

--
-- Now for the translation clause. The translation clause consists
-- guards where a particular guard checks if the next prefered
-- language is a given language in the translation. So we define a
-- function which given a language translation pattern given the
-- corresponding guard.
--

-- | Create a guard with a given language expression.
langGuard :: ExpQ          -- ^ lang variable name
          -> ExpQ          -- ^ mesage variable
          -> LangPatTrans  -- ^ the language definition
          -> Q (Guard, Exp)
langGuard lE msgE (lval,mpats) = normalGE [| $lE == $lval |]
                                          $ mkCase msgE mpats

--
-- Finally we need a guard that iterates through the rest of the
-- preference list.
--

-- | Check rendering with the rest of the languages
langRest :: ExpQ -- ^ master var
         -> ExpQ -- ^ rest of the languages
         -> ExpQ -- ^ message variable
         -> Q (Guard, Exp)
langRest masterE lsE msgE = normalGE [| otherwise |]
                                     [| renderMessage $masterE $lsE $msgE |]

--
-- We are now ready to define the translation clause.
--
--

translateClause :: [LangPatTrans] -> ClauseQ
translateClause ldefs = do
  (masterE,masterP) <- newVarEPat "master"
  (lE     ,     lP) <- newVarEPat "l"
  (lsE    ,    lsP) <- newVarEPat "ls"
  (msgE   ,   msgP) <- newVarEPat "msg"
                      
  clause [masterP
         , conP '(:) [lP,lsP]
         , msgP
         ]
         ( guardedB $  map (langGuard lE msgE) ldefs 
                    ++ [langRest masterE lsE msgE]
         )
         []

  where newVarEPat nm = do v <- newName nm; return (varE v, varP v)


-- | Get the language name from the file name.
langName :: FilePath -> Lang
langName = T.pack . dropExtension . takeFileName

--
-- Finally we put it together in this message instance creating
-- function
--

-- | Check if the file is a valid message file.
isMessageFile :: FilePath -> Bool
isMessageFile f = takeExtension f == (extSeparator:"msg")

-- | The workhorse function for i18n.
mkMessage :: TypeQ          -- ^ For which type
          -> [LangPatTrans] -- ^ The translations, empty for default
                            -- instance.
          -> [MesgPatDef]   -- ^ The default definitions
          -> DecQ
mkMessage msgTyp trans defs = do
        masterT <- varT <$> newName "master"
        mkInstance [] ''RenderMessage [masterT, msgTyp]
                      [funD 'renderMessage cls]
  where cls = if null trans then [defaultClause defs]
                  else [translateClause trans, defaultClause defs]


-- Parsing translations
-- --------------------
--
--
                       


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

-- | Given an administrative interface this function generates the
-- default translation files for the `Action` and `Attribute`
-- associated type of the entity. This function is useful if you want
-- to tweak the default instance. The function will not overwrite the
-- translation file if it already exists. Hence it is safe to put this
-- function in your persistent entity definition.

mkDefaultTransFiles :: FilePath -- ^ Base admin directory
                    -> Lang     -- ^ Which is the default language.
                    -> [AdminInterface]
                    -> Q ()
mkDefaultTransFiles baseFP lang 
  = runIO . sequence_ . map (writeTrans baseFP lang)
   
writeTrans :: FilePath -> Lang -> AdminInterface -> IO ()
writeTrans fp lang ai = do writeButDontOverWrite actPath actTrans
                           writeButDontOverWrite attPath attTrans
      where actPath  = actionTransPath fp ai </> langFile lang
            attPath  = attributeTransPath fp ai </> langFile lang
            actTrans = transText $ defaultActionMesgDef ai
            attTrans = transText $ defaultAttributeMesgDef ai
         
writeButDontOverWrite :: FilePath -> Text -> IO ()
writeButDontOverWrite fp txt 
  = do cond <- doesFileExist fp
       when (not cond) $ do
            createDirectoryIfMissing True $ dropFileName fp
            TIO.writeFile fp txt
                                  
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
