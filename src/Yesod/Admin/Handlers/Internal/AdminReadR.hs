{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.AdminReadR
       ( getAdminReadR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

getAdminReadR :: ( Yesod master
                 , YesodPersist master
                 , b ~ YesodPersistBackend master
                 , PathPiece (Key b v)
                 )
              => Key b v  -- ^ The
              -> AdminHandler master v RepHtml
getAdminReadR k = defaultLayout $ do
              addHamlet [hamlet|Should display object of Id #{toPathPiece k}|]
