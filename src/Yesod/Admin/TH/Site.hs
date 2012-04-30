{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-|

This module sets up the admin sites for all administrable objects of
you application.

-}

module Yesod.Admin.TH.Site
       (
       -- * Basic Idea.
       -- $basicIdea
         mkAdminSite
       -- * Route Constructors.
       -- $adminRoutes
       -- * Default admin home page.
       -- $defaultPage
       -- * Separating dispatch and route construction
       -- $separate
       , mkAdminData
       , mkAdminDispatch
       ) where

import Language.Haskell.TH

import Yesod
import Yesod.Routes.TH


import Yesod.Admin.Types
import Yesod.Admin.Class

-- $basicIdea
--
-- Let @Site@ by your foundation type. The TH code in this module
-- helps create the subsite data type @SiteAdmin@. The crud/selection
-- subsites for each entity will occur as subsites of @SiteAdmin@. All
-- that is left for the user is to hook @SiteAdmin@ as a subsite of
-- the foundation type @Site@.
--


-- | Create an admin subsite.
mkAdminSite :: Bool     -- ^ should generate selection subsite?
            -> String   -- ^ Foundation type
            -> [String] -- ^ Entities
            -> DecsQ
mkAdminSite genSel master ens
            = do aData <- mkAdminData genSel master ens
                 disp  <- mkAdminDispatch genSel master ens
                 return $ disp ++ aData

-- | This function is similar to the `mkAdminSite` function but does
-- not generate the dispatch instance for the site. Useful to separate
-- the definition of dispatch into a different module. Make sure that
-- the constructor to the type safe url where the admin site is to be
-- hooked is defined in the context where the TH function is called.
mkAdminData :: Bool         -- ^ Generate selection site or not
            -> String       -- ^ Foundation type
            -> [String]     -- ^ Entities
            -> DecsQ
mkAdminData genSel master ens
            = do admin <- defAdmin master
                 als   <- mkAdminAliases genSel master ens
                 rR    <- mkRenderRouteInstance aT
                                 $ mkAdminResources genSel ens
                 return $ rR:admin ++ als
   where aT = ConT $ mkName $ adminSiteType master

-- | Creates a dispatch instance for an admin subsite. Used together
-- with `mkAdminData` when seperation of dipatch and route generations
-- are required.
mkAdminDispatch :: Bool      -- ^ Generate the selection subsite or
                             -- not.
                -> String    -- ^ Foundation type
                -> [String]  -- ^ The entities
                -> DecsQ
mkAdminDispatch genSel master ens
     = sequence [instanceD (cxt []) yDispatch
                           [funD 'yesodDispatch [clz]]]
      where  yDispatch = conT ''YesodDispatch `appT` aT `appT` mT
             aT  = conT $ mkName $ adminSiteType master
             mT  = conT $ mkName master
             clz       = mkDispatchClause [|yesodRunner|]
                                          [|yesodDispatch|]
                                          [|fmap chooseRep|]
                                          res
             res       = mkAdminResources genSel ens

-- | This generates the admin aliases for each entry.
mkAdminAliases :: Bool     -- ^ Generate selection subsite or not.
               -> String   -- ^ Foundation type
               -> [String] -- ^ Entities
               -> DecsQ
mkAdminAliases genSel master = fmap concat
                             . sequence
                             . map (mkEntityAdminAliases genSel master)

-- | Generate resources for an admin site given the entity names.
mkAdminResources :: Bool       -- ^ Generate selection site or not.
                 -> [String]   -- ^ Entities
                 -> [Resource Type]
mkAdminResources genSel ens = concatMap mkR ens
    where mkR  = mkEntityResource genSel

mkEntityResource :: Bool   -- ^ Generate selection site or not
                 -> String -- ^ Entity
                 -> [Resource Type]
mkEntityResource genSel en | genSel    = [sr]
                           | otherwise = [cr]
      where mkR c ps ty = Resource c [(True, Static p)| p <- ps]
                          $ Subsite (ConT $ mkName ty)
                                    (getFuncName ty)
            cr = mkR (crudCons en) [en] $ crudTypeName en
            sr = mkR (selCons en)  [en, "selection"] $ selTypeName en

-- $separate
--
-- For a yesod subsite one needs to create the routes and the dispatch.
-- often one needs to separate this out
-- For an entity Foo, the template haskell function `mkAdminData`
-- generates the type aliases FooCrud and, if the selection subsite is
-- required, FooSelection. If your `mkAdminDispatch` is in a different
-- haskell file make sure to export these type aliases.

mkEntityAdminAliases :: Bool   -- ^ Generate selection site or not
                     -> String -- ^ Foundation site
                     -> String -- ^ Entity
                     -> DecsQ
mkEntityAdminAliases genSel master en | genSel    = sequence selA
                                      | otherwise = sequence crudA
   where crudA = [ defCrudType master en
                 , defGet (crudTypeName en) "Crud"
                 ]
         selA  = [ defSelType master en
                 , defGet (selTypeName en) "Selection"
                 ]

homeRes :: Resource Type
homeRes = Resource "AdminHomeR" [] $ Methods Nothing ["GET"]

mkSelAliases :: String  -- ^ Foundation
             -> String  -- ^ Entity name
             -> DecsQ
mkSelAliases master entity = sequence [ defSelType master entity
                                      , defGet (selTypeName entity)
                                               "Selection"
                                      ]

defAdmin   :: String        -- ^ Foundation type
           -> DecsQ
defAdmin master = sequence [ dataD (cxt []) aN [] [ normalC aN [] ] []
                           , defGet aT aT
                           ]
    where aT = adminSiteType master
          aN = mkName aT

adminSiteType :: String -> String
adminSiteType = flip (++) "Admin"

mkCode :: (String -> DecsQ) -> [String] -> DecsQ
mkCode f = combineDecs . map f

combineDecs :: [DecsQ] -> DecsQ
combineDecs = fmap concat . sequence


defCrudType :: String  -- Foundation
            -> String  -- Entity
            -> DecQ
defCrudType site ty = tySynD (mkName $ crudTypeName ty) [] crud
    where crud  = [t|Crud $siteN  $tyN|]
          siteN = conT $ mkName site
          tyN   = conT $ mkName ty

defSelType :: String  -- Foundation
           -> String  -- Entity
           -> DecQ

defSelType site ty = tySynD (mkName $ selTypeName ty) [] sel
    where sel   = [t|Selection $siteN  $tyN|]
          siteN = conT $ mkName site
          tyN   = conT $ mkName ty



defGet :: String        -- ^ Type name
       -> String        -- ^ Constructor
       -> DecQ
defGet ty con = funD name [clause [wildP] (normalB body) []]
       where name = mkName $ getFuncName ty
             body = sigE (conE $ mkName con) $ conT $ mkName ty


getFuncName  :: String -> String
getFuncName = (++) "get"

crudTypeName :: String -> String
selTypeName  :: String -> String

crudTypeName = flip (++) "Crud"
selTypeName  = flip (++) "Selection"

crudCons :: String -> String
selCons  :: String -> String

-- $adminRoutes
--
-- We now describe how to obtain type safe urls to parts of the admin
-- site. Let @Site@ be the foundation type of your application then
-- the TH code generates the subsite @SiteAdmin@ which you can hook
-- into the route of your foundation type @Site@. Let @AdminR@ be the
-- corresponding constructor.
--
crudCons ty = crudTypeName ty ++ "R"
selCons ty  = selTypeName ty ++ "R"
