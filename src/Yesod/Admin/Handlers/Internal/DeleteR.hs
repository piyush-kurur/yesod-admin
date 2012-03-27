{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.DeleteR
       ( postDeleteR
       ) where
import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.Helpers

postDeleteR :: ( Yesod master
               , YesodPersist master
               , PathPiece (SiteKey master v)
               )
            => SiteKey master v  -- ^ The
            -> CrudHandler master v RepHtml
postDeleteR k = defaultLayout $ do 
         toWidget [hamlet|Should delete object of Id #{toPathPiece k}|]
