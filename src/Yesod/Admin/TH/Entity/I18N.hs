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

         mkAdminMessageI18N
       , mkAdminMessageDefault
       , mkDefaultTransFiles

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
import Yesod.Admin.Message
import Yesod.Admin.Helpers.Text
import Yesod.Admin.TH.Helpers
import Yesod.Admin.TH.Error
import Yesod.Admin.TH.Entity.AdminInterface


type Text = T.Text


-- | Given the administrative interface for an list of entities, this
-- TH function creates 'RenderMessage' instance for the types
-- @'Action' v@, @'Attribute' v@ and @'Collective' v@ for each of the
-- entity @v@ in the list. Its first argument is a directory where the
-- translation files of the entities reside (as described in the
-- section directory structure).
mkAdminMessageI18N :: String   -- ^ Master type
                   -> FilePath -- ^ Directory containing the
                               -- translation file
                   -> Lang     -- ^ Default language
                   -> [AdminInterface]
                   -> DecsQ
mkAdminMessageI18N master dir lang ais = concat <$> sequence (mkDef <$> ais)
   where mkDef ai = do
           ascTp <- sequence [ mkAttributeMsg dir lang ai
                             , mkActionMsg dir lang ai
                             ]
           colTp <- mkCollectiveMsg master dir lang ai
           return $ concat [ascTp, colTp]

-- | Generate default "RenderMessage" instances for all entities.
--
mkAdminMessageDefault :: [AdminInterface]
                      -> DecsQ
mkAdminMessageDefault = sequence . concatMap mkDef
   where mkDef ai = [ mkActionMesgDefault ai
                    , mkAttributeMesgDefault ai
                    , mkCollectiveMsgDefault ai
                    ]

mkCollectiveMsg :: String         -- ^ master type name
                -> FilePath
                -> Lang
                -> AdminInterface
                -> DecsQ
mkCollectiveMsg master dir lang ai = do
  (:) <$> typeDef <*> mkMessageFor master typeName path lang

  where typeDef  = tySynD (mkName typeName) []
                          [t| Collective $(conT (mkNameT en)) |]
        en       = name ai
        typeName = T.unpack en <> "Collective"
        path     = collectiveTransPath dir ai

-- | Generate default instance of collective.
mkCollectiveMsgDefault :: AdminInterface
                       -> DecQ
mkCollectiveMsgDefault ai = do
  master <- newName "master"
  mkInstance [] ''RenderMessage [ varT master
                                , [t| Collective $(enType) |]
                                ]
                  [rm]


  where enType = conT $ mkNameT $ name ai
        rm     = funD 'renderMessage [cls]
        cls    = clause [wildP, wildP]
                        (normalB $ varE 'defaultRenderCollective)
                        []

-- | Create 'RenderMessage' instance for @'Action'@.
mkActionMsg :: FilePath
            -> Lang
            -> AdminInterface
            -> DecQ
mkActionMsg dir lang ai = do
    unchecked <- runIO    $ parseMesgDir path
    trans     <- sequence $ checkActionTrans ai <$> unchecked
    if null trans then msgGen [] defMesg
       else maybe (fail err) (msgGen trans) $ lookup lang trans
  where defMesg = defaultActionMesgDef ai
        path    = actionTransPath dir ai
        en      = name ai
        msgType = [t| Action $(conT $ mkNameT en) |]
        msgGen  = mkMsg msgType (actionCons en)
        err     = T.unpack en
                <> ":Action: "
                <> "missing translation file "
                <> langFile lang

-- | Create the default RenderMessage instance for @'Action'@.
mkActionMesgDefault :: AdminInterface
                    -> DecQ
mkActionMesgDefault ai = mkMsg msgTyp (actionCons en) [] defs
  where msgTyp   = [t| Action $(conT $ mkNameT en) |]
        defs     = defaultActionMesgDef ai
        en       = name ai

-- | Create the default RenderMessage instance for Attribute.
mkAttributeMesgDefault :: AdminInterface
                       -> DecQ
mkAttributeMesgDefault ai = mkMsg msgTyp (attributeCons en) [] defs
  where msgTyp   = [t| Attribute $(conT $ mkNameT en) |]
        defs     = defaultAttributeMesgDef ai
        en       = name ai

-- | Create 'RenderMessage' instance for @'Attribute'@.
mkAttributeMsg :: FilePath
               -> Lang
               -> AdminInterface
               -> DecQ
mkAttributeMsg dir lang ai = do
    unchecked <- runIO    $ parseMesgDir path
    trans     <- sequence $ checkAttributeTrans ai <$> unchecked
    if null trans then msgGen [] defMesg
       else maybe (fail err) (msgGen trans) $ lookup lang trans
  where defMesg = defaultAttributeMesgDef ai
        path    = attributeTransPath dir ai
        en      = name ai
        msgType = [t| Attribute $(conT $ mkNameT en) |]
        msgGen  = mkMsg msgType (attributeCons en)
        err     = T.unpack en
                <> ":Action: "
                <> "missing translation file "
                <> langFile lang

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

-- | The default collective definition.
defaultCollectiveMesgDef :: AdminInterface
                         -> [MesgDef]
defaultCollectiveMesgDef ai = [ ("Singular", en)
                              , ("Plural", plural)
                              , ("Collection n@Int", showCol)
                              , ("Range s@Int e@Int n@Int", showRange)
                              ]
  where en      = name ai
        plural  = en <> "s"
        showCol = T.unwords ["#{singularPlural n"
                            , T.pack $ show en
                            , T.pack $ show plural
                            , "}"
                            ]
        showRange = "#{s} to #{e} of "
                  <> showCol


-- | Given a function to generate the constructor name and a message
-- definition, returns message definition pattern.
toMesgPatDef :: (Text -> Text)  -- ^ constructor generator
             -> MesgDef         -- ^ The message definition
             -> MesgPatDef
toMesgPatDef consGen (consName, txt) = ( conP (mkNameT $ consGen consName) []
                                       , textL txt
                                       )

-- | Given a function to generate the constructor name and a language
-- translation, returns the language translation pattern.
toLangPatTrans :: (Text -> Text) -- ^ The constructor generator
               -> LangTrans      -- ^ The language translation
               -> LangPatTrans

toLangPatTrans consGen (lang, mdefs)
  = (textL lang, toMesgPatDef consGen <$> mdefs)

-- | Generate an attribute message definition pattern.
toAttributePatDef :: Text       -- ^ Entity name
                  -> MesgDef    -- ^ The message definition
                  -> MesgPatDef
toAttributePatDef entity = toMesgPatDef $ attributeCons entity

-- | Generate an attribute translation pattern.
toAttributePatTrans :: Text
                    -> LangTrans
                    -> LangPatTrans
toAttributePatTrans entity = toLangPatTrans $ attributeCons entity

-- | Generate an action message definition pattern.
toActionPatDef :: Text        -- ^ Entity name
               -> MesgDef     -- ^ The message definition
               -> MesgPatDef
toActionPatDef entity =  toMesgPatDef $ actionCons entity


-- | Generate an attribute translation pattern.
toActionPatTrans :: Text
              -> LangTrans
              -> LangPatTrans
toActionPatTrans entity = toLangPatTrans $ attributeCons entity



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



--
-- Finally we put it together in this message instance creating
-- function
--

-- | The workhorse function for i18n.
mkMsg :: TypeQ          -- ^ For which type
      -> (Text -> Text) -- ^ Function to create constructor names from
                        -- names.
      -> [LangTrans]    -- ^ The translations, empty for default
                        -- instance.
      -> [MesgDef]      -- ^ The default definitions
      -> DecQ
mkMsg msgTyp consGen trans defs = do
        masterT <- varT <$> newName "master"
        mkInstance [] ''RenderMessage [masterT, msgTyp]
                      [funD 'renderMessage cls]
  where cls = if null trans then [defaultClause defsP]
                 else [translateClause transP, defaultClause defsP]
        transP = toLangPatTrans consGen <$> trans
        defsP  = toMesgPatDef   consGen <$> defs





--
-- Parsing translations
-- --------------------
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
--      The translation files are files whose name consist of the
--      language string ending with an extension "msg". Again, if any
--      of the directory is ommited then the corresponding associated
--      type have the default rendering

-- | Get the language name from the file name.
langName :: FilePath -> Lang
langName = T.pack . dropExtension . takeFileName

-- | Get the language file name from language
langFile :: Lang -> FilePath
langFile lang = T.unpack lang <.> "msg"

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

-- | Path to the tranlation files of collective
collectiveTransPath :: FilePath
                    -> AdminInterface
                    -> FilePath
collectiveTransPath base ai = base </> en  </> "collective"
   where en = T.unpack $ name ai

-- | Check if the file is a valid message file.
isMessageFile :: FilePath -> Bool
isMessageFile f = takeExtension f == (extSeparator:"msg")

-- | Get all the message files.
getMessageFiles :: FilePath -> IO [FilePath]
getMessageFiles dir = filter isMessageFile <$> getDirectoryContents dir

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
parseMesgDir dir = do
   exists <- doesDirectoryExist dir
   if exists then  getMessageFiles dir
                   >>= sequence . map (parseMesg dir)
             else return []


parseMesg :: FilePath -> FilePath -> IO LangTrans
parseMesg dir f   = do cont  <- TIO.readFile (dir </> f)
                       return (langName f, parse cont)
    where fieldValue x = (T.strip u, T.strip $ T.tail v)
                 where (u,v) = T.breakOn ":" x
          parse cont   = [ fieldValue x | x <-  T.lines cont
                                        , not $ T.null $ T.strip x
                         ]

-- | Given a list of administrative interfaces of entities this
-- function generates the default translation files for the
-- entity. This function is useful at the start of the translation
-- process to setup the necessary directories and translation files.
-- The function will not overwrite any translation file if it already
-- exists. Hence, leaving it in your source code is safe.
mkDefaultTransFiles :: FilePath -- ^ Base admin directory
                    -> Lang     -- ^ Which is the default language.
                    -> [AdminInterface]
                    -> Q ()
mkDefaultTransFiles baseFP lang
  = runIO . sequence_ . map (writeTrans baseFP lang)

writeTrans :: FilePath -> Lang -> AdminInterface -> IO ()
writeTrans fp lang ai = do writeButDontOverWrite actPath actTrans
                           writeButDontOverWrite attPath attTrans
                           writeButDontOverWrite colPath colTrans
      where actPath  = actionTransPath fp ai     </> msgFile
            attPath  = attributeTransPath fp ai  </> msgFile
            colPath  = collectiveTransPath fp ai </> msgFile
            msgFile  = langFile lang
            actTrans = transText $ defaultActionMesgDef    ai
            attTrans = transText $ defaultAttributeMesgDef ai
            colTrans = transText $ defaultCollectiveMesgDef ai


writeButDontOverWrite :: FilePath -> Text -> IO ()
writeButDontOverWrite fp txt
  = do cond <- doesFileExist fp
       when (not cond) $ do
            createDirectoryIfMissing True $ dropFileName fp
            TIO.writeFile fp txt

--
--
-- Functions to do translation file checks
-- ---------------------------------------
--
--
--
-- | Check action translations.
checkActionTrans :: AdminInterface
                 -> LangTrans
                 -> Q LangTrans
checkActionTrans ai trans
                 | not $ null err = fail err
                 | otherwise      = return trans
  where err     = errTrans (name ai) "Action" actions trans
        actions = fromJust $ action ai


-- | Check attribute translations.
checkAttributeTrans :: AdminInterface
                    -> LangTrans
                    -> Q LangTrans
checkAttributeTrans ai trans
                    | not $ null err = fail   err
                    | otherwise      = return trans
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
                         else t <> T.intercalate ", " ts
