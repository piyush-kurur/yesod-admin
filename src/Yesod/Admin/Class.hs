{-# LANGUAGE MultiParamTypeClasses     #-}
{-|

Sites with admin interfaces. 

-}
module Yesod.Admin.Class
       ( YesodAdmin(..)
       ) where

import Yesod
import Yesod.Auth

import Yesod.Admin.Subsite
import Yesod.Admin.Types

{-|

This class captures those master sites that have an admin interface to
values of type v. The master site should support
authentication. Therefore, YesodAdmin instance can be declared only
for master sites with authentication. Users might have differing
administrative rights. We capture this via the class function crudOf.
We might need to lookup the data base for administrative permissions
and hence the type of crudOf is a GHandler.

-}

class YesodAuth master => YesodAdmin master v where

