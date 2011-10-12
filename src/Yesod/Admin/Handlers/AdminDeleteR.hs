{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.AdminDeleteR
       ( getAdminDeleteR
       ) where
import Yesod
import Yesod.Admin.Subsite

getAdminDeleteR :: ( Yesod master
                   , YesodPersist master
                   , b ~ YesodPersistBackend master
                   , SinglePiece (Key b v)
                   )
                => Key b v  -- ^ The
                -> AdminHandler master v RepHtml
getAdminDeleteR k = defaultLayout $ do 
               addHamlet [hamlet|Should delete object of Id #{toSinglePiece k}|]
