{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}


{-|

This module defines the resource associated with the admin subsite.
We use the template haskell code exported by the yesod-routes package
to define the @`RenderRoute`@ and @`YesodDispatch`@ instance for the
crud and selection subsites. As a normal user you will never need to
uses these functions.

-}
module Yesod.Admin.Resource
       ( mkAdminRoutes
       , mkAdminDispatch
       ) where

import Control.Applicative
import Yesod hiding (get)
import Language.Haskell.TH
import Yesod.Routes.TH

import Yesod.Admin.Class
import Yesod.Admin.Render
import Yesod.Admin.Message



{-

The routes are

/#ID            -- Read it
/create         -- Creat it
/update/#ID     -- update it
/delete/#ID     -- delete it
/               -- selection list
/#Int           -- nth Page of selection
/action         -- Action page

-}

-- | These resources are meant for sites with a persistent backend
-- which is an instance of @'PersistQuery'@.
selectionResources :: Type      -- ^ The selection backend type.
                   -> String    -- ^ entity
                   -> [ResourceTree Type]
selectionResources backend v = map ResourceLeaf
        [ Resource "CrudR"   []  crudDispatch
        , Resource "ListR"   (string "selection") get
        , Resource "PageR"   [stringP "selection", intP] get
        , Resource "ActionR" (string "action") post
        ]
     where crudDispatch = Subsite (crudType backend v) "getCrud"

-- | These resources are meant for sites with a persistent backend
-- which is an instance of @'PersistStore'@.
crudResources :: Type     -- ^ The crud backend type
              -> String   -- ^ Entity type
              -> [ResourceTree Type]
crudResources backend v = map ResourceLeaf
        [ Resource "ReadR"   key $ get
        , Resource "CreateR" (string "create") $ getPost
        , Resource "UpdateR" (onKey "update" ) $ getPost
        , Resource "DeleteR" (onKey "delete" ) $ post
        ]
        where onKey s = [ stringP s, keyP backend v]
              key     = [ keyP backend v ]

-- | Create the @RenderRoute@ instances for both @Crud@ and
-- @Selection@ subsites.
mkAdminRoutes :: String         -- ^ Backend type (variable)
              -> String         -- ^ Entity
              -> DecsQ
mkAdminRoutes b v
      = concat <$>
        sequence [ mkI (crudType bT v)
                       (crudResources bT v)
                 , mkI (selectionType bT v)
                       $ selectionResources bT v
                 ]
      where mkI  = mkRenderRouteInstance' [ClassP ''PathPiece [keyT bT v]]
            bT   = VarT $ mkName b

-- | Generate dispatch instance for selection and crud subsites.
mkAdminDispatch :: String -> String -> DecsQ
mkAdminDispatch m v
    = sequence [ mkDispatchInstance cCxt crudT mT $ crudResources  b v
               , mkDispatchInstance sCxt selT  mT $ selectionResources b v
               ]
    where sCxt   = cxt $ comCxt ++ [ps, pq, rmAc]
          cCxt   = cxt $ comCxt ++ [ps]
          comCxt = [yp, eq , pp, rmAM, rmAt, hAR]
          yp   = classP ''YesodPersist [mT]
          pp   = classP ''PathPiece     [ kT ]
          ps   = classP ''PersistStore  [bT, crudMonad ]
          pq   = classP ''PersistQuery  [bT, selMonad  ]
          hAR   = classP ''HasAdminRendering [mT]
          rmAM = classP ''RenderMessage [mT, aM        ]
          rmAt = classP ''RenderMessage [mT, attrT     ]
          rmAc = classP ''RenderMessage [mT, actionT   ]
          eq   = equalP bT [t|YesodPersistBackend $mT |]
          
          crudMonad = [t|GHandler $crudT $mT  |]
          selMonad  = [t|GHandler $selT $mT   |]
          kT        = [t|Key $bT $vT|]
          crudT     = return $ crudType b v
          selT      = return $ selectionType b v
          aM        = [t|AdminMessage  |]
          attrT     = [t|Attribute $vT |]
          actionT   = [t|Action    $vT |]
          b         = VarT $ mkName "b"
          bT        = return b
          mT        = varT $ mkName m
          vT        = varT $ mkName v

string :: String -> [ (CheckOverlap, Piece typ) ]
string s = [ stringP s ]

methods :: [String] -> Dispatch typ
methods  = Methods Nothing

get      :: Dispatch typ
post     :: Dispatch typ
getPost  :: Dispatch typ
get      = methods ["GET"]
post     = methods ["POST"]
getPost  = methods ["GET","POST"]

stringP :: String -> (CheckOverlap, Piece typ)
intP    :: (CheckOverlap, Piece Type)
keyP    :: Type           -- ^ The backend type
        -> String         -- ^ The entity
        -> (CheckOverlap, Piece Type)

stringP s        = (True, Static s)
intP             = (True, Dynamic $ ConT ''Int)
keyP backend val = (True, Dynamic $ keyT backend val)

keyT :: Type     -- ^ The backend type
     -> String   -- ^ The entity
     -> Type
keyT backend val = ConT ''Key `AppT` backend `AppT` v
     where v       = VarT $ mkName val

adminType :: Name -> Type -> String -> Type
adminType n b v = (ConT n) `AppT` b `AppT` vT
          where vT = VarT $ mkName v

crudType :: Type -> String -> Type
selectionType :: Type -> String -> Type
crudType      = adminType $ mkName "Crud"
selectionType = adminType $ mkName "Selection"


-- | Generate an instance of @'YesodDispatch'@.
mkDispatchInstance :: CxtQ       -- ^ The context
                   -> TypeQ      -- ^ subsite
                   -> TypeQ      -- ^ master
                   -> [ResourceTree Type] -- ^ The resource
                   -> DecQ
mkDispatchInstance context sub master res = instanceD context
                                                      yDispatch
                                                      [thisDispatch]
        where logger  = mkName "logger"
              loggerE = varE logger
              loggerP = VarP logger
              thisDispatch = do 
                Clause pat body decs <-  mkDispatchClause 
                                         [|yesodRunner   $loggerE |]
                                         [|yesodDispatch $loggerE |]
                                         [|fmap chooseRep|]
                                         res
                return $ FunD 'yesodDispatch [Clause (loggerP:pat) body decs]
              yDispatch = conT ''YesodDispatch `appT` sub `appT` master
