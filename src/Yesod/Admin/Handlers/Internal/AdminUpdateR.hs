{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.AdminUpdateR
       ( getAdminUpdateR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers
getAdminUpdateR :: ( Yesod master
                   , YesodPersist master
                   , PathPiece (SiteKey master v)
                   )
                 => SiteKey master v  -- ^ The
                 -> CrudHandler master v RepHtml
getAdminUpdateR k = defaultLayout $ do
         addHamlet [hamlet|Should display object of Id #{toPathPiece k}|]
