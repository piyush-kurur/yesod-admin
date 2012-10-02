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
       ( AdminInterface(..)
       , mkPersistAdmin
       , mkPersistAdminData
       , deriveInlineDisplay
       , deriveAttributeDisplay
       , deriveAdministrable
       ) where

import Control.Applicative
import qualified Data.Text as T
import Data.Maybe
import Data.Either
import Database.Persist.EntityDef
import Database.Persist.TH (MkPersistSettings(..), mkPersist)
import Language.Haskell.TH

import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.TH.Helpers
import Yesod.Admin.TH.Entity.AdminInterface
import Yesod.Admin.TH.Entity.I18N

type Text = T.Text

-- | This function generates is the Admin aware version of
-- @`mkPersist`@. It generates not only the persistent entities for
-- your application but also declares all the all the required
-- instances required to generate the crud and selection subsites of
-- an entity. Besides it defines a variable @entityInterfaces@, which
-- consists of the the administrative interfaces of all the
-- entities. This can be used latter on when creating the main admin
-- page.
mkPersistAdmin  :: MkPersistSettings -> [EntityDef] -> DecsQ
mkPersistAdmin persistSettings edefs = do
  persist <- mkPersist persistSettings edefs
  ais     <- handleErrs $ entityDefToInterface <$> edefs
  insts   <- mkPersistAdmin' ais
  msgs    <- mkAdminMessageDefault ais
  return  $ persist ++ insts ++ msgs
  

-- | This is similar to @`mkPersistAdmin`@ however it does not
-- generate the `RenderMessage` instance for types @`Attribute`@
-- @`Action`@ and @`Collective`@. Use this if you want to i18n and/or
-- customisation. The variable @entityInterfaces@ is defined by this
-- function which can be used in the function
-- @`mkAdminMessageI18N`@. See module "Yesod.Admin.TH.Entity.I18N" for
-- details.
mkPersistAdminData :: MkPersistSettings -> [EntityDef] -> DecsQ
mkPersistAdminData persistSettings edefs = do
  persist <- mkPersist persistSettings edefs
  ais     <- handleErrs $ entityDefToInterface <$> edefs
  insts   <- mkPersistAdmin' ais
  return  $ persist ++ insts
  

handleErrs :: [Either String b] -> Q [b]
handleErrs es | null errs = return bs
              | otherwise = fail $ unlines errs
  where (errs,bs) = partitionEithers es
        
mkPersistAdmin' :: [AdminInterface]
                -> DecsQ
mkPersistAdmin' ais = sequence $ aisDec : concatMap genCode ais
  where aisDec      = valD (varP $ mkName "entityInterfaces")
                           (normalB $ adminInterfaceList ais)
                           []
        genCode ai  = [ deriveAdministrable' ai
                      , deriveInlineDisplay' ai
                      , deriveAttributeDisplay' ai
                      ]

-- | Generate some admin aliases
        
mkAdminAliases :: MkPersistSettings  -- ^ The persistent setting.
               -> [AdminInterface]   -- ^ admin interfaces.
               -> DecsQ
mkAdminAliases persistSettings = sequence . concatMap (mkA . name)
  where rhs t  n = conT t `appT` backend `appT` conT (mkNameT n)
        selTD  n = tySynD (selectionSubsiteAlias n) [] $ rhs ''Selection n
        crudTD n = tySynD (selectionSubsiteAlias n) [] $ rhs ''Crud n
        mkA    n = [ selTD n , crudTD n ]
        backend = return $ mpsBackend persistSettings


-- | Derive an instance of @`Administrable`@ for the type @v@ given
-- the AdminInterface for @v@.
deriveAdministrable  :: AdminInterface
                     -> DecsQ
deriveAdministrable' :: AdminInterface
                     -> DecQ
deriveAdministrable  = fmap (:[]) . deriveAdministrable'

-- | Derive an instance of `InlineDisplay` for an entity give its
-- administrative interface.
deriveInlineDisplay  :: AdminInterface -> DecsQ
deriveInlineDisplay  = fmap (:[]) . deriveInlineDisplay'


-- | Derive an instance of `AttributeDisplay` for an entity give its
-- administrative interface.
deriveAttributeDisplay :: AdminInterface
                       -> DecsQ
deriveAttributeDisplay = fmap (:[]) . deriveAttributeDisplay'


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


-- | Similar to `deriveInlineDisplay` but does not wrap the
-- declaration inside a list. Not very useful in the wild as splicing
-- expects `DecsQ` instead `DecQ` but useful in defining other
-- template haskell function. Currently not exported.
deriveInlineDisplay' :: AdminInterface
                     -> DecQ
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
           attrDisp = funD 'attributeDisplay $ map mkClause $ attributes ai
           mkClause at = clause [attributeConsP en at] body []
                    where body = normalB $ rhs at
           rhs at | isDerived at = varE $ mkNameT $ attributeFunctionName en at
                  | otherwise    = [|inlineDisplay . $fname|]
                     where fname = varE $ mkNameT $ attributeFieldName en at


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
     where cons = attributeCons en <$> attributes ai
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

{-

This class is not really required; we do not export it. However, this
cleans up some ugly TH code.

-}

class ToTH a where
  toTH :: a -> Q Exp

instance ToTH Text where
  toTH = textL

instance ToTH a => ToTH [a] where
  toTH  = listE . map toTH

instance ToTH a => ToTH (Maybe a) where
  toTH (Just x) =  [e| Just $(toTH x) |]
  toTH (Nothing) = [e| Nothing |]

-- | Wrap an admin interface into TH code.

adminInterfaceToTH :: AdminInterface -> ExpQ
adminInterfaceToTH ai = [e|
      AdminInterface { name         = $(toTH $ name     ai)
                     , action       = $(toTH $ action   ai)
                     , inline       = $(toTH $ inline   ai)
                     , list         = $(toTH $ list     ai)
                     , readPage     = $(toTH $ readPage ai)
                     , dbAttrs      = $(toTH $ dbAttrs  ai)
                     , derivedAttrs = $(toTH $ derivedAttrs ai)
                     }
     |]

adminInterfaceList :: [AdminInterface] -> ExpQ
adminInterfaceList = listE . map adminInterfaceToTH
