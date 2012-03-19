{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ConstraintKinds      #-}
{-|

This module defines the resource associated with the admin subsite.
You will never have to look into this.

-}
module Yesod.Admin.Resource
       ( selectionResources
       , crudResources
       -- * Routes
       -- $routes
       ) where

import Yesod hiding (get)
import Yesod.Admin.Types
import Data.Default
import Yesod.Routes.TH
import Language.Haskell.TH

{-

The routes are

/#ID            -- Read it
/create         -- Creat it
/update/#ID     -- update it
/delete/#ID     -- delete it
/list      -- selection list
/list/#Int -- Page
/action         -- Action page

-}

-- | These resources are meant for sites with a persistent backend
-- which is an instance of "PersistQuery".
selectionResources :: String            -- ^ master site type
                   -> String            -- ^ entity type
                   -> [Resource Type]
selectionResources master v =
        [ Resource "ListR"   (string "list"  ) $ get
        , Resource "PageR"   [stringP "list", intP]
                                       $ get
        , Resource "ActionR" (string "action") $ post
        ]

-- | These resources are meant for sites with a persistent backend
-- which is an instance of "PersistStore".
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

{-
-- | All admin resources.
adminResources :: String        -- ^ master site
               -> String        -- ^ Entity type
               -> [Resource Type]
adminResources master v = crudResources master v
                        ++ selectionResources master v

renderRouteInstance :: String -> String -> DecsQ
renderRouteInstance master v = do instanceD context
                                            renderR
                                            [ mkRenderRoute master v
                                            , mkRoute master v
                                            ]
   where context      = cxt [ yesodPersistP master, pathPiece ]
         pathPiece    = classP ''PathPiece $ [return $ keyT master v]
         renderR = appT (conT ''RenderRoute) $ adminType master v


yesodPersistP :: String -> PredQ
yesodPersistP master = classP ''YesodPersist [ varT $ mkName master]


applyT :: TypeQ -> [TypeQ] -> TypeQ
applyT f = foldl appT f

crudT :: applyT conT ''
{-
adminType :: String -> String -> TypeQ
adminType master v = applyT (conT ''Admin)
                           [ varT $ mkName master
                           , varT $ mkName v
                           ]

-}

-- ^ Generate an instance of YesodDispatch.
mkDispatchInstance :: CxtQ       -- ^ The context
                   -> TypeQ      -- ^ subsite
                   -> TypeQ      -- ^ master
                   -> [Resource Type] -- ^ The resource
                   -> DecQ
mkDispatchInstance context sub master res = instanceD context
                                                   yDispatch
                                                   [ thisDispatch]   
        where clauses  = mkDispatchClause [|yesodRunner|]
                                          [|yesodDispatch|]
                                          [|fmap chooseRep|]
                                          res
              thisDispatch    = funD 'yesodDispatch [clauses]
              yDispatch = [t| YesodDispatch $sub $master |]


dispatchContext :: Name -> String -> String -> CxtQ
dispatchContext backendClass master v
                = cxt [ yesodPersistP master
                      , classP backendClass [ yBackend, handler]
                      , pathpiece
                      ]
     where pathpiece = classP ''PathPiece [ key ]
           key       = applyT (conT ''Key) [ yBackend
                                           , varT $ mkName v
                                           ]
           masterN   = mkName master
           handler   = applyT (conT ''GHandler) [ adminType master v
                                                , varT masterN
                                                ]
           yBackend  = appT (conT ''YesodPersistBackend)
                            $ varT masterN
           

dispatchStore :: String -> String -> DecQ
              -- ^ Create a dispatch instance when the persistent
              -- backend is an instance of store.

dispatchStore master v =
      mkDispatchInstance (dispatchContext ''PersistStore master v)
                         (adminType master v)
                         (varT $ mkName master)
                         $ crudResources master v

dispatchQuery master v =
      mkDispatchInstance (dispatchContext ''PersistQuery master v)
                         (adminType master v)
                         (varT $ mkName master)
                         $ adminResources master v

mkRoute :: String -> String -> DecQ
mkRoute master val = dataInstD (cxt [])
                    ''Route [adminType master val] cons
                            [''Eq, ''Show, ''Read]
   where res    = adminResources master val
         cons   = map return $ mkRouteCons res

mkRenderRoute :: String -> String -> DecQ
mkRenderRoute m v = do clauses <- mkRenderRouteClauses 
                                      $ adminResources m v
                       return $ FunD 'renderRoute clauses

-}

