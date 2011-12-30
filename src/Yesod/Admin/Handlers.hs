{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}
{-|

This module exports some helper handlers.

-}

module Yesod.Admin.Handlers
       ( toMasterRoute
       ) where

import Data.Text (Text)
import Yesod
import Yesod.Auth
import Yesod.Admin.Class
import Yesod.Admin.Types
import Yesod.Admin.Subsite
import Yesod.Admin.Handlers.Helpers

-- | Lifts an admin route to the master route.

toMasterRoute :: Route (Admin master v) -> AdminHandler master v (Route master)
toMasterRoute r = do lft <- getRouteToMaster
                     return $ lft r

type Link       site   = (Text, Route site)

-- | A link group is a set of links with a title.

type LinkGroup  site  = (Text, [Link site])


-- | The default handlers displays the admin sites root page.

getAdminRootPage :: ( HasAdminUser master
                    , HasAdminLayout master
                    )
                 => [LinkGroup sub]
                 -> GHandler sub master RepHtml
getAdminRootPage lgs = withAdminUser $ getAdminRootPage' lgs

getAdminRootPage' :: ( HasAdminUser master
                     , HasAdminLayout master
                     )
                  => [LinkGroup sub]
                  -> AuthId master
                  -> GHandler sub master RepHtml

getAdminRootPage' lgs _
        = do liftR <- getRouteToMaster
             adminLayout $ addHamlet [hamlet|
                      $forall (title,links) <- lgs
                            <div .menu>
                              <div .menu-title> title
                                   $forall (item,route) <- links
                                      <div .menu-item>
                                           <a href=@{liftR route}>#{item}
                       |]     