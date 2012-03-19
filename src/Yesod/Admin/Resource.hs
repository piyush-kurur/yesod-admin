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
       ( adminResources
       , renderRouteInstance
       -- * Routes
       -- $routes
       ) where

import Yesod
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

adminResources :: String -> String -> [Resource Type]
adminResources master v =
        [ Resource "ReadR"   key $ get
        , Resource "CreateR" (string "create") $ getPost
        , Resource "UpdateR" (onKey "update" ) $ getPost
        , Resource "DeleteR" (onKey "delete" ) $ getPost
        , Resource "ListR"   (string "list"  ) $ get
        , Resource "PageR"   page              $ get
        , Resource "ActionR" (string "action") $ post
        ]
  where get      = methods ["GET"]
        getPost  = methods ["GET", "POST"]
        post     = methods ["POST"]
        key      = [ keyP master v ]
        string s = [ stringP s ]
        onKey  s = [ stringP s, keyP master v]
        page     = [ stringP "list", intP]

mkRoute :: String -> String -> Dec
mkRoute master val = DataInstD [] ''Route [admin] cons
                                  [''Eq, ''Show, ''Read]
   where m = VarT $ mkName master
         v = VarT $ mkName val
         admin  = foldl AppT (ConT ''Admin) [m,v]
         res    = adminResources master val
         cons   = mkRouteCons res

mkRenderRoute :: String -> String -> DecQ
mkRenderRoute m v = do clauses <- mkRenderRouteClauses $ adminResources m v
                       return $ FunD 'renderRoute clauses

renderRouteInstance :: String -> String -> DecQ
renderRouteInstance m v = do rr <- mkRenderRoute m v
                             return $ InstanceD [yp, pathp] renderR
                                                [rr, mkRoute m v]
   where yp     = ClassP ''YesodPersist [master]
         pathp  = ClassP ''PathPiece $ [keyT m v]
         master = VarT $ mkName m
         val    = VarT $ mkName v
         adminT = foldl AppT (ConT ''Admin) [master, val]
         renderR = AppT (ConT ''RenderRoute) adminT



methods :: [String] -> Dispatch typ
methods  = Methods Nothing

stringP :: String -> (CheckOverlap, Piece typ)
stringP s = (True, Static s)
intP = (True, Dynamic $ ConT ''Int)


keyP :: String -> String -> (CheckOverlap, Piece Type)
keyP master val = (True, Dynamic $ keyT master val)

keyT :: String -> String -> Type
keyT master val = foldl AppT keyCon [AppT ypb m, v]
     where m       = VarT $ mkName master
           v       = VarT $ mkName val
           ypb     = ConT $ ''YesodPersistBackend
           keyCon  = ConT $ ''Key
