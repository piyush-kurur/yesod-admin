{-# LANGUAGE OverloadedStrings  #-}
{-|

All the messages used by Yesod admin site. Declare an instance of
RenderMessage for AdminMessage in your site to get i18n.

-}

module Yesod.Admin.Message
       ( AdminMessage(..)
       , ObjectCollection(..)
       , defaultRenderObjectCollection
       ) where

import Database.Persist.Store
import Database.Persist.EntityDef
import Yesod.Admin.Helpers.Text

import qualified Data.Text as T

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

-- | Datatype representing a collection of objects. There are four
-- forms: (1) to describe number of objects, for example when one
-- wants to say something like 10 users, (2) the singular form, for
-- example user, (3) The plural form e.g. users and (4) when you want
-- to give an object range, e.g. 1 to 10 of 100 users. Define a
-- @`RenderMessage`@ instance for this type to get i18n support. See
-- also the function @`defaultRenderObjectSet`@
data PersistEntity v => ObjectCollection v
     = Objects Int      -- ^ numeric form. E.g 10 users
     | ObjectSingular   -- ^ Singular form. E.g user
     | ObjectPlural     -- ^ Collective form. E.g users
     | ObjectRange Int Int Int -- ^ E.g. 1 to 20 of 100 users

-- | Use this function to generate a default instance of
-- @`RenderMessage'@ for an object set.
defaultRenderObjectCollection :: PersistEntity v
                              => ObjectCollection v
                              -> T.Text
defaultRenderObjectCollection = render undefined
     where render :: PersistEntity v
                  => v
                  -> ObjectCollection v
                  -> T.Text
           render v os = case os of
                  Objects 1 -> T.unwords [ "1"
                                         , singular
                                         ]
                  Objects n -> T.unwords [ showT n
                                         , plural
                                         ]
                  ObjectSingular    -> singular
                  ObjectPlural      -> plural
                  ObjectRange s e t -> T.unwords [ showT s
                                                 , "to"
                                                 , showT e
                                                 , "of"
                                                 , render v (Objects t)
                                                 ]
               where singular = unCamelCase
                              . unHaskellName
                              . entityHaskell
                              $ entityDef v
                     plural   = singular `T.append` "s"

showT :: Show a => a -> T.Text
showT = T.pack . show
