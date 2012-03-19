{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

{-

Declares the yesod dispatch instance for the admin subsite. This is
autogenerated from the Y.A.Resource via Template haskell. So expect an
ugly Haddock.

-}

module Yesod.Admin.Dispatch() where

import Data.Text(Text)

import Yesod
import Yesod.Admin.Types
import Yesod.Admin.Resource
import Yesod.Admin.Routes
import Yesod.Admin.Handlers.Internal

mkAdminDispatch "master" "v"
