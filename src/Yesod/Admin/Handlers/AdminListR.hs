{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}

{-|

-}
module Yesod.Admin.Handlers.AdminListR
       ( getAdminListR
       ) where

import Yesod
import Text.Hamlet
import Text.Cassius

import Yesod.Admin.Class
import Yesod.Admin.Crud
import Yesod.Admin.Subsite
import Yesod.Admin.Types
import Yesod.Admin.Handlers.Helpers


getAdminListR :: ( Yesod master
                 , YesodAdmin master v
                 , YesodPersist master
                 , b ~ YesodPersistBackend master
                 , m ~ GGHandler (Admin master v) master IO
                 , PersistEntity v
                 , PersistBackend b m
                 )
                 => AdminHandler master v RepHtml
getAdminListR = withCrud listing


listing :: ( Yesod master
           , YesodPersist master
           , b ~ YesodPersistBackend master
           , m ~ GGHandler (Admin master v) master IO
           , PersistEntity v
           , PersistBackend b m
           )
       => AdminCRUD master v
       -> AdminHandler master v RepHtml

listing crud = do values <- runDB $ selectList filters sorting
                  defaultLayout $ do addHamlet $(hamletFile "templates/listing.hamlet")
                                     addCassius $(cassiusFile "templates/listing.cassius")
                                     addCassius $(cassiusFile "templates/buttons.cassius")
       where ListingStyle headers func = listingStyle crud
             filters    = listingFilter crud
             sorting    = listingSort crud
             {- FIXME: How to lift these to the master route
                readR    (k,_) = AdminReadR k
                deleteR  (k,_) = AdminDeleteR k
                modifyR  (k,_) = AdminUpdateR k
             -}
             columnOf (_,val) = func val
