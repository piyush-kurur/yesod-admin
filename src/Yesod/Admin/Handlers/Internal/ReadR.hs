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
import Yesod.Admin.Class
import Yesod.Admin.Render

getReadR :: ( Yesod master
            , YesodPersist master
	    , HasAdminRendering master
            , b ~ YesodPersistBackend master
            , PathPiece (Key b v)
            )
          => Key b v             -- ^ The Id of the object to read
          -> CrudHandler master v RepHtml
getReadR k = defaultLayout $ do
       toWidget [hamlet|Should display object of Id #{toPathPiece k}|]
