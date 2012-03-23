{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.CreateR
       ( getCreateR
       , postCreateR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

getCreateR :: Yesod master
           => CrudHandler master v RepHtml
getCreateR = defaultLayout $ addHamlet [hamlet|should support creation|]

postCreateR :: Yesod master
            => CrudHandler master v RepHtml
postCreateR = defaultLayout $ addHamlet [hamlet|should support creation|]
