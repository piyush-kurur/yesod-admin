{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.ReadR
       ( getReadR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

getReadR :: ( Yesod master
            , YesodPersist master
            , b ~ YesodPersistBackend master
            , PathPiece (Key b v)
            )
          => Key b v  -- ^ The
          -> CrudHandler master v RepHtml
getReadR k = defaultLayout $ do
       addHamlet [hamlet|Should display object of Id #{toPathPiece k}|]