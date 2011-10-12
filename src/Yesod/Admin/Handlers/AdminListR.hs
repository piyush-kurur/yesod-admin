{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}

{-|

-}
module Yesod.Admin.Handlers.AdminListR
       ( getAdminListR
       ) where

import Yesod
import Yesod.Admin.Subsite

getAdminListR :: Yesod master => GHandler (Admin master v) master RepHtml
getAdminListR = defaultLayout $ do addHamlet [hamlet|Should list all objects|]
