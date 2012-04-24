{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

{-|

This module declares the @`RenderRoute`@ for @`Admin` master v@.


-}

module Yesod.Admin.Routes where

import Yesod
import Yesod.Admin.Resource

mkAdminRoutes "master" "v"
