{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}

{-|

This module defines the handler for the /page/ route.

-}
module Yesod.Admin.Handlers.Internal.AdminActionR
       ( postAdminActionR
       ) where

import Data.Text(Text)
import Yesod
import Yesod.Auth
import Yesod.Admin.Subsite
import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.Render
import Yesod.Admin.Handlers.Helpers

