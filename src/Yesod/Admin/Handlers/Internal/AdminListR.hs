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
import Yesod.Admin.Handlers.Internal.AdminPageR



getAdminListR :: Yesod master
               => AdminHandler master v RepHtml
getAdminListR  = getAdminPageR 0
