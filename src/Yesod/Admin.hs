{-|

This is the main module that one needs for building admin sites for
database objects. 

-}

module Yesod.Admin
       ( module Yesod.Admin.Subsite
       , module Yesod.Admin.Class
       , module Yesod.Admin.Types
       , module Yesod.Admin.Render
       , module Yesod.Admin.TH
       ) where

import Yesod.Admin.Subsite
import Yesod.Admin.Class
import Yesod.Admin.Types
import Yesod.Admin.Render
import Yesod.Admin.TH
import Yesod.Admin.Dispatch
