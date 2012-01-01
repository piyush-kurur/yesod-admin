{-|

The standard way to give persistence to yesod sites is via the
persistent package. To build an admin interfaces for persistent
entities of your site import this module.

-}

module Yesod.Admin
       (
       -- * Admin site.
       -- $concepts
     
       -- * Basic usage.
       -- $basic

       -- * Configuring the admin interfaces.
       -- $configuring

       -- ** Access control.
       -- $accesscontrol

       -- * Linking to admin pages.
       -- $linking

       -- * Advanced usage (mostly for developers).
       -- $advanced

         module Yesod.Admin.Subsite
       , module Yesod.Admin.Class
       , module Yesod.Admin.Types
       , module Yesod.Admin.Handlers
       , module Yesod.Admin.Render
       , module Yesod.Admin.TH
       ) where

import Yesod.Admin.Subsite
import Yesod.Admin.Class
import Yesod.Admin.Types
import Yesod.Admin.Handlers
import Yesod.Admin.Render
import Yesod.Admin.TH
import Yesod.Admin.Dispatch

{- $concepts

The goal of an admin interface is to provide basic CRUD
operations. Any non-trivial site would also want to have access
controls on these operations. The goal of this module is to provide
such an interface.

Before we go into the details of how this is done let us explain few
concepts that are relevant for effective use of the admin site.

   * Column display: In an admin site we would want to list all the
     objects of a particular kind. Such lists will be shown as a table
     with columns showing attributes of the object. By /column
     display/ we mean such a display. The attributes shown in the
     display will be called /administrative columns/ or just
     /columns/.

   * Column title: Each column of an object has /column title/ which
     shows what the attribute is.
   
   * Constructed columns or derived columns: The /administrative
     columns/ of an object /need not/ be a database column of an
     object. For example, one might be storing the date of birth
     inside the database but would like to show the age of a person.
     An administrative column which is /not/ a database column will be
     called a /constructed/ or /derived/ column.

   * Inline display: Often we would also want to displaying objects in
     running text or even inside columns of a particular object.  This
     mode of display is called inline display. Just like columns an
     inline display need not show a database column of the entity.
     For example to avoid duplication while showing people, one might
     want to use name together with email address.

An admin interface should give ways of configuring both the column
display and the inline display.

-}

{- $basic 

We give a quick discription of how use this library. Firstly make sure
that your master site is an instances of

   1. YesodPersist: Clearly you need this otherwise what will you
      administer.

   2. YesodAuth: What does access control mean without authentication

The very first step is to define the admin layout to use. The default
admin skin should be fine so declare the following instance

> instance HasAdminLayout Site where

Now you need to tell who are the users who admininstrative access
site.

> instance HasAdminUsers Site where
>          isSuperUser authid = ....
>          isAdminUser authid = ....

Both 'isSuperUser authid' and 'isAdminUser authid' are your site
handlers (i.e. 'GHandler' sub Site Bool) returns a 'Bool' indicating
whether the users are super users or admin users. For more details
check "Yesod.Admin.Class"

Now you need to perpare each entity to have an admininstrative
interfaces. We explain this with an example. Let us say you have a
entity defined as follows.

>      [persist|
>                Person
>                        name    Text
>                        email   Text
>                        age     Text
>                        address Text
>     |]

You can define the admin interfaces by the following code

>
> mkYesodAdmin (simpleAdmin "nameAndEmail" :: AdminInterface Person)
> nameAndEmail person = return $ Text.concat [ personName person
>                                            , "<", personEmail person, ">"
>                                            ]

Here the @nameAdminEmail@ function is used both in the inline display
as well as in the column display. Notice that this is not a database
column of Person.

The function `mkYesodAdmin` declares all the instances and other
sundry like the type alias @PersonAdmin@ (it is an alias to @`Admin`
Site Person@) and the function @getPersonAdmin@. You can then hook the
admin site of person to the main site as follows.

> mkYesod "Site" [parseRoutes|
>                / RootR GET
>                ...
>                /admin/person/ PersonAdminR PersonAdmin getPersonAdmin

-}

{- $accesscontrol

The default access control gives admin privileges to super users of
your site and no one else. If this is not what is desired you need to
declare the 'YesodAdmin' class instance for your entity. There is a
variant `mkEntityAdmin` which does every thing that `mkYesodAdmin`
does but for the declaration of the `YesodAdmin` instance. Use this
variant and declare the `YesodAdmin` instance by hand. For more
details refer to the module "Yesod.Admin.Class".

-}

{- $configuring

Clearly @simpleAdmin@ is not always sufficient. For example say, we
want to display name and age in column displays of Person. We could
change this by changing the above code as follows

> mkYesodAdmin (simpleAdmin "nameAndEmail" 
>                          { lisiting = ["Name", "Age"] }
>                          :: AdminInterface Person
>             )

In both lisiting and inline we follow the convention that
   
   * Strings starting with an upper case letter denote database columns.

   * Strings starting with a lower case letter denotes a constructed column.
     In this case you should have a function with the same name which returns
     the textual representation.
     
For more details on what all can be configured check out the
documentation of the `AdminInterface` type

-}

{- $linking

Often one would want to give links to the admin page from various
location. The Yesod way of giving links is via type safe urls. For
example to give a link to the listing of Person in a hamlet file
you could use something like

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

     * Admin site routes: To know about the admin subsite and the various
       routes refer the module "Yesod.Admin.Subsite"

     * Type aliases: The module "Yesod.Admin.Types" have some type aliases
       that can shorten some type signatures.

-}
