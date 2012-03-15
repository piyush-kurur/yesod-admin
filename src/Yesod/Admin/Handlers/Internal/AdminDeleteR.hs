{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.AdminDeleteR
       ( getAdminDeleteR
       ) where
import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

getAdminDeleteR :: ( Yesod master
                   , YesodPersist master
                   , PathPiece (SiteKey master v)
                   )
                => SiteKey master v  -- ^ The
                -> AdminHandler master v RepHtml
getAdminDeleteR k = defaultLayout $ do 
               addHamlet [hamlet|Should delete object of Id #{toPathPiece k}|]
