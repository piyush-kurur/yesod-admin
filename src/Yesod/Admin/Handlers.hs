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
                                               
-- | Create an entry corresponding to a admin.
defaultAdminRootEntry :: ( YesodPersist master
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
defaultAdminRootEntry selectR createR v =
       toWidget [hamlet| <div .entity-name>
                              <a href=@{selectR} _{plural v}
                         <a href=@{createR}>
                            <div .button .create> _{MsgAdminCreate}
                |]
     where plural :: PersistEntity e => e -> Collective e
           plural _ = Plural


-- | Create an entry on the admin page when only crud is supported.
defaultAdminCrudEntry :: ( YesodPersist master
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
defaultAdminCrudEntry createR v =
       toWidget [hamlet| <div .entity-name> _{plural v}
                         <a href=@{createR}>
                            <div .button .create> _{MsgAdminCreate}
                |]
     where plural :: PersistEntity e => e -> Collective e
           plural _ = Plural

                              
-- | Given a set of entries creates an admin page out of this.
defaultAdminRootPage :: HasAdminLayout master
              => [GWidget sub master ()]
              -> GHandler sub master RepHtml
defaultAdminRootPage ws = adminLayout [whamlet|
                                  <ul>
                                      $foreach w <- ws
                                           <li .entity-item>
                                               ^{w}
                               |]
