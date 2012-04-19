{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}
{-|

This module exports some helper handlers.

-}

module Yesod.Admin.Handlers
       ( toMasterRoute
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Subsite
                                               
-- | Default homepage for admin sites.

getAdminRootEntry :: ( YesodPersist master
                     , b ~ YesodPersistBackend master
                     , m ~ GHandler sub master
                     , PersistQuery b m
                     , Administrable v
                     , RenderMessage master (Collective v)
                     , RenderMessage master AdminMessage
                     )
                   => Route master      -- ^ Selection route
                   -> Route master      -- ^ Create route
                   -> v                 -- ^ The entity
                   -> GWidget sub master ()

getAdminRootEntry selectR createR v =
       toWidget [hamlet| <div .entity-name>
                              <a href=@{selectR} _{plural v}
                         <a href=@{selectR}>
                            <div .button .create> _{MsgAdminCreate}
                |]
     where plural :: PersistEntity e => e -> Collective e
           plural _ = Plural

-- | This generates an admin page which each entry being the 
getAdminRootR :: HasAdminLayout master
              => [GWidget sub master ()]
              -> GHandler sub master RepHtml
getAdminRootR ws = adminLayout [whamlet|
                                  <ul>
                                      $foreach w <- ws
                                           <li .entity-item>
                                               ^{w}
                               |]
