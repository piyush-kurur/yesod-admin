{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.CreateR
       ( getCreateR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

getCreateR :: Yesod master
                => AdminHandler master v RepHtml
getCreateR = defaultLayout $ addHamlet [hamlet|should support creation|]
