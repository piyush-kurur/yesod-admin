{-|

All the messages used by Yesod admin site. Declare an instance of
RenderMessage for AdminMessage in your site to get i18n.

-}

module Yesod.Admin.Message
       ( AdminMessage(..)
       ) where

import Yesod.Admin.Class

-- | Messages used on Admin pages. You would need to generate a
-- @RenderMessage@ instance for this in your master site.

data AdminMessage = MsgAdminCreate
                  | MsgAdminRead
                  | MsgAdminUpdate
                  | MsgAdminDelete
                  | MsgAdminSelect
                  | MsgAdminGo
                  | MsgAdminConfirm
                  | MsgAdminCancel
                  | MsgAdminLogin
                  | MsgAdminLogout
                  | MsgAdminObjectRange Int Int Int
