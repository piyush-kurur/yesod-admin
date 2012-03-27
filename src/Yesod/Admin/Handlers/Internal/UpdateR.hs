{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.UpdateR
       ( getUpdateR
       , postUpdateR
       ) where

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers
getUpdateR :: ( Yesod master
              , YesodPersist master
              , PathPiece (SiteKey master v)
              )
            => SiteKey master v  -- ^ The
            -> CrudHandler master v RepHtml
getUpdateR k = defaultLayout $ do
       addHamlet [hamlet|Should display update form of objectId #{toPathPiece k}|]

postUpdateR :: ( Yesod master
              , YesodPersist master
              , PathPiece (SiteKey master v)
              )
            => SiteKey master v  -- ^ The
            -> CrudHandler master v RepHtml

postUpdateR k = defaultLayout $ do
       toWidget [hamlet|Should update objectId #{toPathPiece k}|]
