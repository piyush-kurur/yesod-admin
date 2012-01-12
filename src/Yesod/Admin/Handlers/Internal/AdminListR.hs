{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.AdminListR
       ( getAdminListR
       ) where

import Yesod

import Yesod.Admin.Class
import Yesod.Admin.Subsite
import Yesod.Admin.Types


getAdminListR :: ( Yesod master
                 , YesodAdmin master v
                 , YesodPersist master
                 , b ~ YesodPersistBackend master
                 , m ~ GGHandler (Admin master v) master IO
                 , PersistEntity v
                 , PersistBackend b m
                 )
               => AdminHandler master v RepHtml
getAdminListR  = do liftR   <- getRouteToMaster
                    redirect RedirectPermanent $ liftR $ AdminPageR 0
