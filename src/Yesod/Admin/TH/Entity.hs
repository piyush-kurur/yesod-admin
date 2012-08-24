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
         mkAdminInstances
       , deriveInlineDisplay
       , deriveAttributeDisplay
       , deriveAdministrable
       ) where

import Control.Applicative
import qualified Data.Text as T
import Data.Maybe
import Database.Persist.EntityDef
import Language.Haskell.TH

import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.TH.Helpers
import Yesod.Admin.TH.Entity.AdminInterface

type Text = T.Text

-- | To use the crud and selection subsites of an entity, we need to
-- derive a few instances for an entity. This combinator derives a
-- default instance for all the entities specified in its
-- argument. Typically you would want to use this with the persist
-- quasi-quoter.
mkAdminInstances :: [EntityDef] -> DecsQ
mkAdminInstances edefs =  mkAdminInstances' edefs

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




