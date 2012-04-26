{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-|

This module sets up the admin sites for all administrable objects of
you application. Let @Site@ by your foundation type then the TH code
here helps create the subsite data type @SiteAdmin@. The crud and
selection subsites for each entity will occur as subsites of
@SiteAdmin@. All that is left for the user is to hook @SiteAdmin@ as a
subsite of the foundation type @Site@.


-}

module Yesod.Admin.TH.Site
       ( mkAdminSite
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

-- | Create an admin subsite.
mkAdminSite :: Bool     -- ^ should generate selection subsite?
            -> String   -- ^ Foundation type
            -> String   -- ^ The constructor of the url where admin
                        -- site is hooked. E.g AdminR
            -> [String] -- ^ Entities
            -> DecsQ
mkAdminSite genSel master adminR ens
            = do aData <- mkAdminData genSel master adminR ens
                 disp  <- mkAdminDispatch' genSel master ens
                 return $ disp:aData

-- | This function is similar to the `mkAdminSite` function but does
-- not generate the dispatch instance for the site. Useful to separate
-- the definition of dispatch into a different module. Make sure that
-- the constructor to the type safe url where the admin site is to be
-- hooked is defined in the context where the TH function is called.
mkAdminData :: Bool         -- ^ Generate selection site or not
            -> String       -- ^ Foundation type
            -> String       -- ^ Admin root constructor
            -> [String]     -- ^ Entities
            -> DecsQ
mkAdminData genSel master adminR ens
            = do admin <- defAdmin master
                 als   <- mkAdminAliases genSel master ens
                 lCs   <- mkCode (mkLiftCrudRoutes master adminR) ens
                 rR    <- mkRenderRouteInstance aT
                                 $ mkAdminResources genSel ens
                 return $ rR:concat [admin, als, lCs]
   where aT = ConT $ mkName $ adminSiteType master

-- | Creates a dispatch instance for an admin subsite. Used together
-- with `mkAdminData` when seperation of dipatch and route generations
-- are required.
mkAdminDispatch :: Bool      -- ^ Generate the selection subsite or
                             -- not.
                -> String    -- ^ Foundation type
                -> [String]  -- ^ The entities
                -> DecsQ
mkAdminDispatch genSel master = fmap (:[]) . mkAdminDispatch' genSel master

-- | The dispatch instance generating workhorse.
mkAdminDispatch' :: Bool     -- ^ Whether to create selection subsite
                 -> String   -- ^ Foundation type
                 -> [String] -- ^ Entities
                 -> DecQ
mkAdminDispatch' genSel master = genAdminDispatch mT aT
                               . mkAdminResources genSel
      where aT  = conT $ mkName $ adminSiteType master
            mT  = conT $ mkName master

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
mkAdminResources genSel ens = homeRes : concatMap mkR ens
    where mkR  = mkEntityResource genSel

mkEntityResource :: Bool   -- ^ Generate selection site or not
                 -> String -- ^ Entity
                 -> [Resource Type]
mkEntityResource genSel en | genSel    = [cr,sr]
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
mkEntityAdminAliases genSel master en | genSel    = sequence $ crudA ++ selA
                                      | otherwise = sequence crudA
   where crudA = [ defCrudType master en
                 , defGet (crudTypeName en) "Crud"
                 ]
         selA  = [ defSelType master en
                 , defGet (selTypeName en) "Selection"
                 ]


genAdminDispatch :: TypeQ    -- ^ Admin type
                 -> TypeQ    -- ^ master type
                 -> [Resource Type]
                 -> DecQ
genAdminDispatch mT aT res = instanceD (cxt []) yDispatch
                                       [funD 'yesodDispatch [clz]]
      where  yDispatch = conT ''YesodDispatch `appT` aT `appT` mT
             clz       = mkDispatchClause [|yesodRunner|]
                                          [|yesodDispatch|]
                                          [|fmap chooseRep|]
                                          res


homeRes :: Resource Type
homeRes = Resource "AdminHomeR" [] $ Methods Nothing ["GET"]

mkSelAliases :: String  -- ^ Foundation
             -> String  -- ^ Entity name
             -> DecsQ
mkSelAliases master entity = sequence [ defSelType master entity
                                      , defGet (selTypeName entity)
                                               "Selection"
                                      ]

mkLiftCrudRoutes :: String      -- ^ Foundation type
                 -> String      -- ^ admin Root constructor
                 -> String      -- ^ Entity
                 -> DecsQ

mkLiftCrudRoutes master adminR entity
                 = [d| instance LiftCrudRoutes $mT $eT where
                                liftCrudR = $adminRC . $crudC
                   |]
             where eT       = conT $ mkName entity
                   mT       = conT $ mkName master
                   adminRC  = conE $ mkName adminR
                   crudC    = conE $ mkName $ crudCons entity

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

-- $type aliase
--
getFuncName  :: String -> String
getFuncName = (++) "get"

crudTypeName :: String -> String
selTypeName  :: String -> String

crudTypeName = flip (++) "Crud"
selTypeName  = flip (++) "Selection"

crudCons :: String -> String
selCons  :: String -> String

crudCons ty = crudTypeName ty ++ "R"
selCons ty  = selTypeName ty ++ "R"



