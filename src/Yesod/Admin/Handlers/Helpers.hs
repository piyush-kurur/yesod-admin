{-# LANGUAGE QuasiQuotes #-}
{-

Here we define some helper functions that are required by all
handlers.


-}

module Yesod.Admin.Handlers.Helpers where

import Text.Hamlet
import Yesod
import Yesod.Auth
import Yesod.Admin.Class
import Yesod.Admin.Subsite
import Yesod.Admin.Types

-- | This function checks whether the authenticated user is an admin user. It 
-- gives the login page if not authenticated and checks admin permissions if
-- the user is already authenticated.
withAdminUser :: ( HasAdminUser master
                 , HasAdminLayout master
                 )
              => (AuthId master -> GHandler sub master RepHtml) -- the action
              -> GHandler sub master RepHtml
withAdminUser action = do aid <- requireAuthId
                          allow <- isAdminUser aid
                          if allow then action aid
                             else do adminPermissionDenied

adminPermissionDenied :: HasAdminLayout master
                      => GHandler sub master RepHtml
adminPermissionDenied = adminLayout $ addHtml
                                    [shamlet|
                                        You dont have administrative privileged
                                    |]
