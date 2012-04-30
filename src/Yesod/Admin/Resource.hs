{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}


{-|

This module defines the resource associated with the admin subsite.
You will never have to look into this.

-}
module Yesod.Admin.Resource
       ( mkAdminRoutes
       , mkAdminDispatch
       ) where

import Yesod hiding (get)
import Yesod.Admin.Class
import Yesod.Admin.Message
import Data.Default
import Yesod.Routes.TH
import Language.Haskell.TH

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
selectionResources :: String    -- ^ master
                   -> String    -- ^ entity
                   -> [Resource Type]
selectionResources master v =
        [ Resource "CrudR"   []  crudDispatch
        , Resource "ListR"   (string "selection") get
        , Resource "PageR"   [stringP "selection", intP] get
        , Resource "ActionR" (string "action") post
        ]
     where crudDispatch = Subsite (crudType master v)
                                  "getCrud"


-- | These resources are meant for sites with a persistent backend
-- which is an instance of @'PersistStore'@.
crudResources :: String   -- ^ master site
              -> String   -- ^ Entity type
              -> [Resource Type]
crudResources master v =
        [ Resource "ReadR"   key $ get
        , Resource "CreateR" (string "create") $ getPost
        , Resource "UpdateR" (onKey "update" ) $ getPost
        , Resource "DeleteR" (onKey "delete" ) $ post
        ]
        where onKey s = [ stringP s, keyP master v]
              key     = [ keyP master v ]

-- | Create the @RenderRoute@ instances for both @Crud@ and
-- @Selection@ subsites.
mkAdminRoutes :: String -> String -> DecsQ
mkAdminRoutes master v
      = sequence [ mkI (crudType master v)
                       (crudResources master v)
                 , mkI (selectionType master v)
                       $ selectionResources master v
                 ]
      where mkI       = mkRenderRouteInstance' context
            context   = [ yp, pathPiece ]
            pathPiece = ClassP ''PathPiece
                               [ keyT master v]
            yp        = ClassP ''YesodPersist
                               [ VarT $ mkName master ]

-- | Generate dispatch instance for selection and crud subsites.
mkAdminDispatch :: String -> String -> DecsQ
mkAdminDispatch m v = sequence [ dispatchCrud m v
                               , dispatchSelection m v
                               ]


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
keyP    :: String -> String -> (CheckOverlap, Piece Type)

stringP s       = (True, Static s)
intP            = (True, Dynamic $ ConT ''Int)
keyP master val = (True, Dynamic $ keyT master val)

keyT :: String -> String -> Type
keyT master val = foldl AppT keyCon [AppT ypb m, v]
     where m       = VarT $ mkName master
           v       = VarT $ mkName val
           ypb     = ConT $ ''YesodPersistBackend
           keyCon  = ConT $ ''Key




adminType :: Name -> String -> String -> Type
adminType n m v = foldl AppT (ConT n)
                        $ map (VarT . mkName) [m,v]


crudType :: String -> String -> Type
selectionType :: String -> String -> Type
crudType      = adminType $ mkName "Crud"
selectionType = adminType $ mkName "Selection"


-- | Generate an instance of @'YesodDispatch'@.
mkDispatchInstance :: CxtQ       -- ^ The context
                   -> TypeQ      -- ^ subsite
                   -> TypeQ      -- ^ master
                   -> [Resource Type] -- ^ The resource
                   -> DecQ
mkDispatchInstance context sub master res = instanceD context
                                                   yDispatch
                                                   [thisDispatch]
        where clauses  = mkDispatchClause [|yesodRunner|]
                                          [|yesodDispatch|]
                                          [|fmap chooseRep|]
                                          res
              thisDispatch    = funD 'yesodDispatch [clauses]
              yDispatch = conT ''YesodDispatch `appT` sub `appT` master

-- | Generate dispatch instance for @'Crud'@ subsite.
dispatchCrud :: String -> String -> DecQ
dispatchCrud m v = mkDispatchInstance c crudT mT $ crudResources m v
   where c        = cxt $ commonPredQs m v
         crudT    = return $ crudType m v
         mT       = varT $ mkName m

-- | Generate dispatch instance for @'Selection'@ subsite.
dispatchSelection :: String -> String -> DecQ
dispatchSelection m v = mkDispatchInstance c selT mT
                            $ selectionResources m v
   where c       = cxt $ commonPredQs m v ++ [pq, rmAc]
         pq      = classP ''PersistQuery [ yBackend, handler]
         selT    = return $ selectionType m v
         mT      = varT $ mkName m
         vT      = varT $ mkName v
         rmAc    = classP ''RenderMessage [ mT, actionT]
         actionT =  [t| Action $vT |]
         yBackend = [t| YesodPersistBackend $mT |]
         handler  = [t| GHandler $selT $mT |]

commonPredQs :: String -> String -> [PredQ]
commonPredQs m v  = [yp, pp, ps, rmAM, rmAt]
   where yp       = classP ''YesodPersist [ mT ]
         pp       = classP ''PathPiece    [ key ]
         ps       = classP ''PersistStore [ yBackend, handler]
         rmAM     = classP ''RenderMessage [ mT, conT ''AdminMessage ]
         rmAt     = classP ''RenderMessage [ mT, attributeT]
         key      = [t| Key (YesodPersistBackend $mT) $vT |]
         mT       = varT $ mkName m
         vT       = varT $ mkName v
         attributeT = [t| Attribute $vT |]
         yBackend = [t| YesodPersistBackend $mT |]
         handler  = [t| GHandler $crudT $mT |]
         crudT    = return $ crudType m v
