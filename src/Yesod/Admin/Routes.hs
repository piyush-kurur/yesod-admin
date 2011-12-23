{-|

This module contains some helper functions to access certain admin
routes.


-}

module Yesod.Admin.Routes
       ( toMasterRoute
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Subsite

-- | Lifts an admin route to the master route.

toMasterRoute :: Route (Admin master v) -> AdminHandler master v (Route master)
toMasterRoute r = do lft <- getRouteToMaster
                     return $ lft r

