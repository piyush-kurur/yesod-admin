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

getAdminUpdateR :: ( Yesod master
                   , YesodPersist master
                   , b ~ YesodPersistBackend master
                   , PathPiece (Key b v)
                   )
                 => Key b v  -- ^ The
                 -> AdminHandler master v RepHtml
getAdminUpdateR k = defaultLayout $ do
         addHamlet [hamlet|Should display object of Id #{toPathPiece k}|]
