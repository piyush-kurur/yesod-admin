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
