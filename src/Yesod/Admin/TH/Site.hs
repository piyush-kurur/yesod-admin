{-# LANGUAGE TemplateHaskell    #-}
{-| 

This module sets up the admin sites for all administrable objects of
you application. Say you have a foundation type @Foundation@ and
suppose that you have already declared the admin sites for the
individual entities say Foo, Bar and Biz, either by hand or using the
functions in "Yesod.Admin.TH.Entity". Now you would want to hook this
into the url of the main site. By using the TH code @'mkSiteAdmin'
\"Foundation\" [\"Foo\", \"Bar\", \"Biz\"]@, you can create subsite
@FoundationAdmin@ creates the subsite with the following routes


> /foo         FooAdminR FooAdmin getFooAdmin
> /bar         BarAdminR BarAdmin getBarAdmin
> /biz         BizAdminR BarAdmin getBarAdmin

You can then hook the subsite @FoundationAdmin@ at an appropriate
place in your main sites route.

-}

module Yesod.Admin.TH.Site
       (
       ) where

import Language.Haskell.TH
import Yesod.Admin.Helpers
import Yesod.Admin.Subsite

-- | The TH expression @`defAdmin "Site" "Foo"` results in the
-- following definitions
--
-- > type FooAdmin = Admin Site Foo
-- > getFooAdmin   :: Site -> FooAdmin
-- > getFooAdmin _ = getAdmin
--
--

defAdmin :: String -- ^ the site's foundation type
         -> String -- ^ the persistent entity
         -> DecsQ
defAdmin site entity = sequence [ tySynD alias [] admin
                                , sigD funcName funcType
                                , funD funcName [body]
                                ]
     where admin       = conT ''Admin `appT` siteTy
                                      `appT` entityTy
           alias       = mkName $ entityAdmin entity
           funcType    = arrowT `appT` siteTy
                                 `appT` conT alias
           funcName    = mkName $ getEntityAdmin entity
           body        = clause [wildP] (normalB  $ varE 'getAdmin) []
           siteTy    = conT $ mkName site   -- the TH type of site
           entityTy  = conT $ mkName entity -- the TH type of the entity

-- | This function creates the route string for a given type

routeString :: String  -- ^ the persist entity
            -> String
routeString entity = unwords [ "/" ++ unCapitalise entity
                             , entityR     entity
                             , entityAdmin entity
                             , getEntityAdmin entity
                             ]

-- | The admin routes for a given given site.

routes site entities = unlines  $ root:rest
       where root = unwords ["/", siteAdminR site,  "GET"]
             rest = map routeString entities

getEntityAdmin :: String  -- ^ the persistent entity
               -> String
getEntityAdmin entity = "get" ++ entity ++ "Admin"

entityAdmin :: String -- ^ the persistent entity
            -> String
entityAdmin entity = entity ++ "Admin"

entityR   :: String -- ^ the persist entity
          -> String
entityR entity = entity ++ "AdminR"

siteAdminR :: String  -- ^ the site admin route
           -> String
siteAdminR site = site ++ "AdminR"



{-
-- | This TH function makes all but the main admin page handler.

mkSiteAdmin :: String   -- ^ the site's foundation type
            -> [String] -- ^ the entities for which admin is sought
            -> DecsQ

mkSiteAdmin = 

-}
