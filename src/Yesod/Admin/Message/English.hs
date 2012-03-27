{-# LANGUAGE OverloadedStrings #-}
{-|

Module supporting rendering of admin messages in English.

-}

module Yesod.Admin.Message.English
       ( render
       ) where

import Yesod.Admin.Message
import qualified Data.Text as T

-- | Render the admin messages in English.

render :: AdminMessage -> T.Text

render MsgAdminCreate  = "Create"
render MsgAdminRead    = "View"
render MsgAdminUpdate  = "Modify"
render MsgAdminDelete  = "Delete"
render MsgAdminSelect  = "Select"
render MsgAdminGo      = "Go"
render MsgAdminConfirm = "Confirm"
render MsgAdminCancel  = "Cancel"
render MsgAdminLogin   = "Login"
render MsgAdminLogout  = "Logout"
render (MsgAdminObjectRange s e t) = T.unwords [ showT s
                                               , "to"
                                               , showT e
                                               , "of"
                                               , showT t
                                               , objs
                                               ]
   where showT = T.pack . show 
         objs  = if t /= 0 then "objects"
                 else "object"
