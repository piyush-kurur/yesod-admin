{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-

Here we define some helper functions that are required by all
handlers.


-}

module Yesod.Admin.Handlers.Internal.Helpers where

import Yesod
import Yesod.Auth
import Yesod.Admin.Class
import Yesod.Admin.Subsite

import Data.Text (Text, unpack)


-- | Text to display on permission denied pages.
permissionDeniedText :: Text
permissionDeniedText = "You don't have Admin permissions"

-- | This function checks whether the authenticated user is an admin
-- user. It gives the login page if not authenticated and checks admin
-- permissions if the user is already authenticated.

withAdminUser :: ( HasAdminUsers master
                 , Monad (YesodDB sub master)
                 )
              => (AuthId master -> GHandler sub master RepHtml) -- the action
              -> GHandler sub master RepHtml
withAdminUser action = do aid <- requireAuthId
                          allow <- runDB $ isAdminUser aid
                          if allow then action aid
                             else permissionDenied permissionDeniedText
runDBWithAuthId :: ( HasAdminUsers master
                   , Monad (YesodDB sub master) 
                   )
                => (AuthId master -> YesodDB sub master a)
                -> GHandler sub master a
runDBWithAuthId action = do aid <- requireAuthId
                            runDB $ do allow <- isAdminUser aid
                                       if allow then action aid
                                          else fail $ unpack permissionDeniedText