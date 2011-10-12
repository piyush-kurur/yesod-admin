{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}

{-

-}
module Yesod.Admin.Handlers.AdminPageR
       ( getAdminPageR
       ) where

import Yesod
import Yesod.Admin.Subsite

getAdminPageR ::  Yesod master
              => Int   -- ^ The page to view
              -> AdminHandler master v RepHtml
getAdminPageR x = defaultLayout $ do 
              addHamlet [hamlet|This should show the #{x} th page of listing|]
