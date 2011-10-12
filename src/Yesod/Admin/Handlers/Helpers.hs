{-

Here we define some helper functions that are requrired by all
handlers.


-}

module Yesod.Admin.Handlers.Helpers where



import Yesod.Admin.Class
import Yesod.Admin.Crud
import Yesod.Admin.Subsite

{-

An admin action can be executed if and only if the person is
authenticated.

-}

type AdminAction master v a = AdminCRUD master v -> AdminHandler master v a

withCrud :: YesodAdmin master v
         => AdminAction master v a
         -> AdminHandler master v a

withCrud action =  do crud <- getCrud
                      action crud
