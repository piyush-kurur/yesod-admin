{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-|

This module defines the handler for the /page/ route.

-}
module Yesod.Admin.Handlers.Internal.ActionR
       ( postActionR
       ) where

import Data.Text(Text)
import Yesod
import Yesod.Auth
import Yesod.Admin.Subsite
import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.Render
import Yesod.Admin.Handlers.Helpers

postActionR :: ( Yesod master
               , YesodPersist master
               , b ~ YesodPersistBackend master
               , m ~ AdminHandler master v
               , PersistQuery b m
               )
             => AdminHandler master v RepHtml

postPageR  = defaultLayout $ do
           addHamlet [hamlet| should run the given action|]
