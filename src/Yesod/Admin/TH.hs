{-|

This module contains the template haskell routines to build admin
interfaces.

-}

module Yesod.Admin.TH
       ( module Yesod.Admin.TH.Entity
       , module Yesod.Admin.TH.Site
       ) where

{-

Developers notes:

The th code is divided as follows


[Entity:] Entity specific code goes here. Should have TH code to
generate all the admin classes and message instances.

[Site:] The code to link the admin sites to the main site.


-}

import Yesod.Admin.TH.Entity
import Yesod.Admin.TH.Site
