{-# LANGUAGE MultiParamTypeClasses     #-}
{-|

Sites with admin interfaces. 

-}
module Yesod.Admin.Class
       ( YesodAdmin(..)
       ) where

import Yesod
import Yesod.Auth

import Yesod.Admin.Crud
import Yesod.Admin.Subsite

{-|

This class captures those master sites that have an admin interface to
values of type v. The master site should support
authentication. Therefore, YesodAdmin instance can be declared only
for master sites with authentication. Users might have differing
administrative rights. We capture this via the class function crudOf.

-}

class YesodAuth master => YesodAdmin master v where
      crudOf :: AuthId master -> Maybe (CRUD (Admin master v) master v)
             -- ^ The crud operations allowed for this user. Returns
             -- Nothing if the user is not authorised to administer v.
