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
                 => (Route (Admin master v) -> Route master)
                           -- ^ How to lift routes to the master site
                 -> AdminHandler master v RepHtml
getAdminListR rLift = withCrud $ listing rLift


listing :: ( Yesod master
           , YesodPersist master
           , b ~ YesodPersistBackend master
           , m ~ GGHandler (Admin master v) master IO
           , PersistEntity v
           , PersistBackend b m
           )
       => (Route (Admin master v) -> Route master)
       ->  AdminCRUD master v
       -> AdminHandler master v RepHtml

listing rLift crud = do values <- runDB $ selectList filters sorting
                        defaultLayout $ 
                                do addHamlet $(hamletFile "templates/listing.hamlet")
                                   addCassius $(cassiusFile "templates/listing.cassius")
                                   addCassius $(cassiusFile "templates/buttons.cassius")
       where ListingStyle headers func = listingStyle crud
             filters    = listingFilter crud
             sorting    = listingSort crud
             readR    (k,_) = rLift $ AdminReadR k
             deleteR  (k,_) = rLift $ AdminDeleteR k
             modifyR  (k,_) = rLift $ AdminUpdateR k
             columnOf (_,val) = func val
