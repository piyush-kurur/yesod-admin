{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-|

This module sets up the admin sites for all administrable objects of
you application. Let @Site@ by your foundation type then the TH code
here helps create the subsite data type @SiteAdmin@. The crud and
selection subsites for each entity will occur as subsites of
@SiteAdmin@. All that is left for the user is to hook @SiteAdmin@ as a
subsite of the foundation type @Site@.


-}

module Yesod.Admin.TH.Site
       (

       -- * Type Aliases.
       -- $typeAliases
       -- * Route Constructors.
       -- $adminRoutes
       -- * Default admin home page.
       -- $defaultPage
         mkAdminSite
       , mkAdminData
       , mkAdminDispatch
       -- * Crud only sites.
       -- $crudonly
       , mkAdminCrudSite
       , mkAdminCrudData
       , mkAdminCrudDispatch
       ) where

import Language.Haskell.TH

import Yesod
import Yesod.Routes.TH


import Yesod.Admin.Types
import Yesod.Admin.Class

-- | Create an admin subsite which has the crud and selection subsites
-- of all entity under it.
mkAdminSite :: String   -- ^ Foundation type
            -> String   -- ^ The constructor of the url where admin
                        -- site is hooked. E.g AdminR
            -> [String] -- ^ Entities
            -> DecsQ
mkAdminSite master adminR ens
            = do aData <- mkAdminData master adminR ens
                 disp  <- mkAdminDispatch' master ens
                 return $ disp:aData

-- | This function is similar to the `mkAdminSite` function but does
-- not generate the dispatch instance for the site. Useful to separate
-- the definition of dispatch into a different module. Make sure that
-- the constructor to the type safe url where the admin site is to be
-- hooked is defined in the context where the TH function is called.
mkAdminData :: String       -- ^ Foundation type
            -> String       -- ^ Admin root constructor
            -> [String]     -- ^ Entities
            -> DecsQ
mkAdminData master adminR ens
            = do admin <- defAdmin master
                 als   <- mkCode mkBoth ens
                 lCs   <- mkCode (mkLiftCrudRoutes master adminR) ens
                 rR    <- mkRenderRouteInstance aT
                                 $ mkAdminResources ens
                 return $ rR:concat [admin, als, lCs]
   where mkBoth en = do c <- mkCrudAliases master en
                        s <- mkSelAliases master en
                        return $ c ++ s
         aT = ConT $ mkName $ adminSiteType master

-- | Creates a dispatch instance for an admin subsite.
mkAdminDispatch :: String    -- ^ Foundation type
                -> [String]  -- ^ The entities
                -> DecsQ
mkAdminDispatch master = fmap (:[]) . mkAdminDispatch' master

-- | Similar to `mkAdminDispatch` but generates a DecQ instead of
-- DecsQ.
mkAdminDispatch' :: String   -- ^ Foundation type
                 -> [String] -- ^ Entities
                 -> DecQ
mkAdminDispatch' master = genAdminDispatch mT aT . mkAdminResources
      where aT  = conT $ mkName $ adminSiteType master
            mT  = conT $ mkName master


-- | Similar to `mkAdminSite` but creats and admin subsite with only
-- the crud subsite for each entity hooked in. If your persistent
-- backend does not support the selection (i.e. it is not an instance
-- @PersistQuery@) use this function instead of `mkAdminSite`. However
-- this will limit what actions you can do with your admin subsite.
mkAdminCrudSite :: String   -- ^ Foundation type
                -> [String] -- ^ Entities
                -> DecsQ
mkAdminCrudSite master ens
            = do aData <- mkAdminCrudData master ens
                 disp  <- mkAdminCrudDispatch' master ens
                 return $ disp:aData

-- | Similar to @mkAdminData@ except that only the crud instances are
-- created.
mkAdminCrudData :: String       -- ^ Foundation type
                -> [String]     -- ^ Entities
                -> DecsQ
mkAdminCrudData master ens = 
            do admin <- defAdmin master
               code  <- mkCode (mkCrudAliases master) ens
               rR    <- mkRenderRouteInstance aT
                                 $ mkAdminCrudResources ens
               return $ rR : (admin ++ code)
   where aT = ConT $ mkName $ adminSiteType master


-- | Similar to @mkAdminDispatch@ except that dispatch works only for
-- curd subiste. This should be used together with 'mkAdminCurdData'.
mkAdminCrudDispatch :: String   -- ^ Foundation type
                    -> [String] -- ^ Entities
                    -> DecsQ
mkAdminCrudDispatch master = fmap (:[]) . mkAdminCrudDispatch' master

mkAdminCrudDispatch' :: String   -- ^ Foundation type
                     -> [String] -- ^ Entities
                     -> DecQ
mkAdminCrudDispatch' master = genAdminDispatch mT aT
                           . mkAdminCrudResources
      where aT  = conT $ mkName $ adminSiteType master
            mT  = conT $ mkName master

-- | Create type aliases for crud sites.
mkCrudAliases :: String -- ^ Foundation
              -> String -- ^ Entity
              -> DecsQ
mkCrudAliases master entity = sequence [ defCrudType master entity
                                       , defGet (crudTypeName entity) "Crud"
                                       ]


-- | Create a resource corresponding to the selection subsite of an
-- entity.
mkCrudResource :: String   -- ^ Entity
               -> Resource Type
mkCrudResource entity
               = mkResource (crudCons entity)
                            [entity]
                            (crudTypeName entity)


-- | Create a resource corresponding to the crud subsite of an entity.
mkSelResource :: String     -- ^ Entity
              -> Resource Type
mkSelResource entity
              = mkResource (selCons entity)
                           [entity, "selection"]
                           (selTypeName entity)

-- | Generate resources for an admin site given the entity names.
mkAdminResources :: [String]             -- ^ Entities
                 -> [Resource Type]
mkAdminResources ens = homeRes: concatMap mkR ens
    where mkR en = [ mkCrudResource en
                   , mkSelResource  en
                   ]

-- | Similar to `mkAdminResources` except that only the crud resources
-- are created.
mkAdminCrudResources :: [String]             -- ^ Entities
                     -> [Resource Type]      -- ^ 
mkAdminCrudResources ens = homeRes: map mkCrudResource ens
     
genAdminDispatch :: TypeQ    -- ^ Admin type
                 -> TypeQ    -- ^ master type
                 -> [Resource Type]
                 -> DecQ
genAdminDispatch mT aT res = instanceD (cxt []) yDispatch
                                       [funD 'yesodDispatch [clz]]
      where  yDispatch = [t|YesodDispatch $aT $mT |]
             clz       = mkDispatchClause [|yesodRunner|]
                                          [|yesodDispatch|]
                                          [|fmap chooseRep|]
                                          res


mkResource :: String   -- ^ The constructor
           -> [String] -- ^ The route pieces
           -> String   -- ^ The subsite type
           -> Resource Type
mkResource cons ps ty = Resource cons [(True, Static p) | p <- ps]
                                $ Subsite (ConT $ mkName ty)
                                          (getFuncName ty)

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
{-

mkAdminSite master adminR entities = do c  <- code
                                        d  <- dispInst
                                        r  <- rendRoute
                                        return (r:d:c)
       where (ccs,crs) = unzip $ map (mkCrud master adminR) entities
             (scs,srs) = unzip $ map (mkSel master) entities
             res       = concat [ [c,s] | (c,s) <- zip crs srs ]
             code      = fmap concat $ sequence $ ccs ++ scs
             adminN    = mkName $ adminSiteType master
             adminType = ConT adminN
             mT        = conT $ mkName master
             aT        = return adminType
             rendRoute = mkRenderRouteInstance adminType res
             dispInst  = instanceD (cxt []) yDispatch [adminDispatch]
             yDispatch = [t|YesodDispatch $aT $mT |]
             clz       = mkDispatchClause [|yesodRunner|]
                                          [|yesodDispatch|]
                                          [|fmap chooseRep|]
                                          res
             adminDispatch = funD 'yesodDispatch [clz]

-}



{-


-}




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



