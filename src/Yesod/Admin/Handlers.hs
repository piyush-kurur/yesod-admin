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

-- | Lifts an admin route to the master route.

toMasterRoute :: Route (Admin master v) -> AdminHandler master v (Route master)
toMasterRoute r = do lft <- getRouteToMaster
                     return $ lft r
