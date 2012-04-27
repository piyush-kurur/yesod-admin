{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

{-|

This module declares the @`RenderRoute`@ for @`Admin` master v@.


-}

module Yesod.Admin.Routes where

import Yesod

import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.Resource

mkAdminRoutes "master" "v"

-- | Type safe url to the read page of an object.
readR :: LiftCrudRoutes master v
      => SiteKey master v      -- ^ The id of the object.
      -> Route master
readR = liftCrudR . ReadR

-- | Type safe url to the creation page. The argument is not used at
-- all and is there to get the types right. You may pass an undefined
-- value as input.
createR :: LiftCrudRoutes master v
        => v              -- ^ A dummy value to fix types (can be undefined)
        -> Route master
createR u = liftCrudR $ cr u
        where cr :: v -> Route (Crud master v)
              cr _ = CreateR

-- | Type safe url to the update page of an entity.
updateR :: LiftCrudRoutes master v
        => SiteKey master v      -- ^ The id of the object.
        -> Route master
updateR = liftCrudR . UpdateR

-- | Type safe url to the delete page of an entity.
deleteR :: LiftCrudRoutes master v
        => SiteKey master v      -- ^ The id of the object.
        -> Route master
deleteR = liftCrudR . DeleteR

-- | Type safe url to the selection list of an entity. The argument is
-- not used at all and is there to get the types right. You may pass
-- an undefined value as input.
listR :: LiftSelectionRoutes master v
      => v              -- ^ A dummy value to fix types (can be undefined)
      -> Route master
listR u = liftSelectionR $ lR u
      where lR :: v -> Route (Selection master v)
            lR _ = ListR
