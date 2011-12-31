{-# LANGUAGE TemplateHaskell    #-}

{-| 

This module sets up the admin sites for all administrable objects of
you application. Say you have a foundation type @Foundation@ and
suppose that you have already declared the admin sites for the
individual entities say Foo, Bar and Biz, either by hand or using the
functions in "Yesod.Admin.TH.Entity". Now you would want to hook this
into the url of the main site. We would like to hook it to the main site
as follows

> ... other routes
> /admin            FoundationAdminR GET
> /admin/foo        FooAdminR FooAdmin getFooAdmin
> /admin/bar        BarAdminR BarAdmin getBarAdmin
> /admin/biz        BizAdminR BarAdmin getBarAdmin

This module has TH codes to do this hooking.

FIXME:

Currently we have not implemented it. Waiting for yesod's resource
declaration to be more flexible. 

-}

module Yesod.Admin.TH.Site where



 


