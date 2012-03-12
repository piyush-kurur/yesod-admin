{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.AdminCreateR
       ( getAdminCreateR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

getAdminCreateR :: Yesod master
                => CrudHandler master v RepHtml
getAdminCreateR = defaultLayout $ addHamlet [hamlet|should support creation|]
