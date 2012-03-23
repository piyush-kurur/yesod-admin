{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}

{-|

-}
module Yesod.Admin.Handlers.Internal.ListR
       ( getListR
       ) where

import Yesod

import Yesod.Admin.Class
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Internal.PageR



getListR :: ( Yesod master
            , YesodPersist master
            , b ~ YesodPersistBackend master
            , m ~ SelectionHandler master v
            , PersistQuery b m
            )
         => SelectionHandler master v RepHtml
getListR = getPageR 0
