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
import Yesod.Admin.Subsite
import Yesod.Admin.Types

getAdminReadR :: ( Yesod master
                 , YesodPersist master
                 , b ~ YesodPersistBackend master
                 , SinglePiece (Key b v)
                 )
              => Key b v  -- ^ The
              -> AdminHandler master v RepHtml
getAdminReadR k = defaultLayout $ do
              addHamlet [hamlet|Should display object of Id #{toSinglePiece k}|]
