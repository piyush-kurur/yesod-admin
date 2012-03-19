{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.DeleteR
       ( getDeleteR
       ) where
import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

getDeleteR :: ( Yesod master
              , YesodPersist master
              , PathPiece (SiteKey master v)
              )
            => SiteKey master v  -- ^ The
            -> AdminHandler master v RepHtml
getDeleteR k = defaultLayout $ do 
         addHamlet [hamlet|Should delete object of Id #{toPathPiece k}|]
