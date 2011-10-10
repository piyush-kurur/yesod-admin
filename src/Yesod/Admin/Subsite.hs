{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-|

This defines the admin subsite data type. 

-}
module Yesod.Admin.Subsite
       ( Admin
       , getAdmin
       ) where

import Yesod

-- | The foundation type for admin subsite of type v on the site
-- master.

data Admin master v = Admin

-- | Get a default instance here.

getAdmin :: Admin master v
getAdmin = Admin
