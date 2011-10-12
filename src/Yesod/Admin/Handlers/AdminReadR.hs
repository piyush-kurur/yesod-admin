{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.AdminReadR
       ( getAdminReadR
       ) where

import Yesod
import Yesod.Admin.Subsite

getAdminReadR :: ( Yesod master
                 , YesodPersist master
                 , b ~ YesodPersistBackend master
                 , SinglePiece (Key b v)
                 )
              => Key b v  -- ^ The
              -> GHandler (Admin master v) master RepHtml
getAdminReadR k = defaultLayout $ do
              addHamlet [hamlet|Should display object of Id #{toSinglePiece k}|]
