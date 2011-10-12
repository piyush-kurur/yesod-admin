{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}

{-|

-}
module Yesod.Admin.Handlers.AdminCreateR
       ( getAdminCreateR
       ) where

import Yesod
import Yesod.Admin.Subsite

getAdminCreateR :: Yesod master
                => AdminHandler master v RepHtml
getAdminCreateR = defaultLayout $ addHamlet [hamlet|should support creation|]
