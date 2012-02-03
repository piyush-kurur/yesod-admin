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

getAdminDeleteR :: ( Yesod master
                   , YesodPersist master
                   , b ~ YesodPersistBackend master
                   , PathPiece (Key b v)
                   )
                => Key b v  -- ^ The
                -> AdminHandler master v RepHtml
getAdminDeleteR k = defaultLayout $ do 
               addHamlet [hamlet|Should delete object of Id #{toPathPiece k}|]
