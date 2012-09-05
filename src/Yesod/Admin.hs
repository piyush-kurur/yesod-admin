{-|

The standard way to give persistence to yesod sites is via the
persistent package. To build an admin interfaces for persistent
entities of your site import this module.

-}

module Yesod.Admin
       (
       -- * Usage.
       -- $usage
      
{-
       -- ** Access control.
       -- $accesscontrol

       -- * Advanced usage (mostly for developers).
       -- $advanced

-}
         module Yesod.Admin.Class
       , module Yesod.Admin.Types
       , module Yesod.Admin.TH
       -- , module Yesod.Admin.Dispatch
       , module Yesod.Admin.Routes
       , module Yesod.Admin.Render
       --, module Yesod.Admin.Handlers
       --, module Yesod.Admin.TH
       ) where

import Yesod.Admin.Class
import Yesod.Admin.Types
import Yesod.Admin.Render
import Yesod.Admin.TH

import Yesod.Admin.Routes
import Yesod.Admin.Dispatch()
import Yesod.Admin.TH.Entity.AdminInterface()


{- $usage

We give a quick description of how to use this library. Firstly make
sure that your master site is an instances of

   1. YesodPersist: Clearly you need this otherwise what will you
      administer.

   2. YesodAuth: What does access control mean without authentication?


You then need to prepare your site for admin operations. This involves
two steps:

   1. Setting up a layout for your admin site and
   2. Declaring who are the administrative users of your site.

You can set up the layout by declaring your site to be an instance of
the class `HasAdminRendering`. The default declaration will give you
the default skin and that should be good enough for testing. However,
feel free to change the layout.

> instance HasAdminRendering Site where  -- use the default skin



You can declare the administrative users of your site by declaring
your site to be an instance of `HasAdminUsers`. The default
declaration declares all users as non-admin users so you will most
probably need to redeclare the member functions.

> instance HasAdminUsers Site where
>          isSuperUser authid = ....  -- define who are the super user
>          isAdminUser authid = ....  -- define users who have some
>                                     -- administrative privileges


Both @`isSuperUser` authid@ and @`isAdminUser` authid@ are database
actions, i.e @`YesodDB` sub Site Bool@, that returns a boolean value
indicating whether the input userid are super users or admin users
respectively. For details check the documentation @`HasAdminUsers`@.

Each persistent entity for which the administrative interface is
desired is to be prepared next. The simplest way to achieve this is to
use the template haskell function @`mkAdminClasses`@. The
administrative properties of an entity is controlled by the optional
section named \"Admin\". If the admin section is not present a default
admin interfaces is created. In the example below, the type @Person@
will be listed using two fields name and age. For the type @Group@ the
default administrative setting is used. Details on how to modify
administrative setting is available in the documentation for the
module "Yesod.Admin.TH.Entity.AdminInterface".

> share [ mkPersistAdmin
>       , mkPersist sqlSettings
>       , mkMigrate "migrateAll"
>       ]
>       [persist|
>                Person
>                        name    Text
>                        email   Text
>                        age     Text
>                        address Text
>                        Admin
>                               list name age
>                Group
>                       name    text
>     |]


Finally you need to hook all the administrative subsites of your
persistent entities to your main site. The template haskell functions
in the module "Yesod.Admin.TH.Site" is for that purpose.


-}

{- $concepts

This module provides a way to perform admin actions on Persistent
objects of your site.

   * Selection: In an admin site we would want to list all the objects
     of a particular kind. Such lists will be shown as a table with columns
     showing attributes of the object. By /listings/ we mean such a
     display.

   * Attribute title: Attributes have /attribute title/ which is
     shown in the header of a listing.
   
   * Constructed attributes or derived attributes: The attributes of
     an object /need not/ be a database column of an object. For
     example, date of birth might be a database column, however we
     would like to show the age of a person instead of DOB in
     listings. In this case the age is a derived attribute. An
     attribute which is /not/ a database column will be called a
     /constructed/ or /derived/ attribute.

   * Inline display: Often we would also want to displaying objects in
     running text or even inside attributes of a particular object.
     This mode of display is called inline display. Just like
     attributes an inline display need not show a database column of
     the entity.  For example to avoid duplication while showing
     people, one might want to use name together with email address.

An admin interface should give ways of configuring both the listing
and the inline display.

-}


{- $linking

Often one would want to give links to the admin page from various
location. The Yesod way of giving links is via type safe urls. For
example to give a link to the listing of Person in a hamlet file you
could use something like

> [hamlet| Go to the <a href=@{PersonAdminR AdminListR}>
>          list of persons
> |]

The `AdminListR` is one of the constructors of the type `Route (Admin
Site Person)`. For more details refer the module "Yesod.Admin.Subsite".

-}

{- $advanced

You will almost never have to go beyond the code that is generated by
the template haskell functions. However, if you want to do some
non-standard stuff or would like to be a developer of yesod admin feel
free to go over the rest of the modules. Here are some of the modules
that you need to know in case you want to do something non-standard.

     * Changing the admin sites appearance: Check the class
       `HasAdminLayout` defined in the "Yesod.Admin.Class" module. In
       case you want to design a new admin skin check out the module
       "Yesod.Admin.Render"

     * Hand generation of admin code: You will need to refer to the
       modules "Yesod.Admin.Class" mostly.

     * Admin site routes: To know about the admin subsite and the
       various routes refer the module "Yesod.Admin.Subsite"

     * Type aliases: The module "Yesod.Admin.Types" have some type
       aliases that can shorten some type signatures.

-}
