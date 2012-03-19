{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.UpdateR
       ( getUpdateR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers
getUpdateR :: ( Yesod master
              , YesodPersist master
              , PathPiece (SiteKey master v)
              )
            => SiteKey master v  -- ^ The
            -> AdminHandler master v RepHtml
getUpdateR k = defaultLayout $ do
       addHamlet [hamlet|Should display object of Id #{toPathPiece k}|]
