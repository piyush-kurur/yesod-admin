{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE UndecidableInstances     #-}


{-|

This module collects various type classes associated with admin
subsites. Normally you need not bother too much about this module; you
would be using this via the template haskell code available inside the
module "Yesod.Admin.TH". However, in case you are a developer or want
to do something non-standard, this is the module to refer to.

The admin interfaces of an object is controlled by four classes
defined here.

* The class 'Administrable'. This class is used to define things like
  name of the object the attributes in listings of the object etc.

* The class 'InlineDisplay'. This class controls how a particular
  object is showed in inline text.

* The class 'AttributeDisplay'. This class controls what is shown in
  the listing of a particular object.

* The class 'YesodAdmin'. This class defines access control and forms
  for the object.

Master sites need to be instances of 'YesodAuth' and also need to
specify who have administrative previledges to the site. This is done
via the class 'HasAdminUser'

The last class that is defined here is the 'HasAdminLayout'. This can
be used to control the rendering of the admin site. If you want to
change the appearence of the site, change the branding this is what
you configure.

-}
module Yesod.Admin.Class
       ( Administrable(..)
       , InlineDisplay(..)
       , AttributeDisplay(..)
       , HasAdminUser (..)
       , HasAdminLayout (..)
       , YesodAdmin(..)
       ) where

import Data.Int
import Data.ByteString (ByteString)
import Data.Text(Text, append, pack)
import System.Locale
import Data.Time
import Yesod
import Yesod.Auth
import Database.Persist.Base
import Yesod.Admin.Helpers
import Yesod.Admin.Subsite
import Yesod.Admin.Types
import Yesod.Admin.Render
import Yesod.Admin.Render.Defaults
import Text.Hamlet

{-| 

    This class captures objects that have an admin interface.  The key
member is the associated type @'Attribute'@. An attribute in the
listing of an object can either be a database column of an object or
might be a constructed entity. Each attribute in the listing has a
title. The member function @'attributeTitle'@ maps an attribute of a
data type @`v`@ (of type @'Attribute' v@) to its title. Minimum
complete definition of this class requires defining the associated
type @'Attribute' v@ and the function @'AttributeTitle'@.

-}

class PersistEntity v => Administrable v where

      -- | The name of the object. Used in various places for example
      -- in titles of admin pages. The default values is the entity
      -- name of the given persistent type.
      objectSingular :: v -> Text
      objectSingular = pack . unCamelCase . entityName . entityDef

      -- | The plural form of the object. By default an `s' is
      -- appended to the singular form.
      objectPlural  :: v -> Text
      objectPlural  v = objectSingular v `append` "s"

      -- | Abstract attributes of the type. These attributes can
      -- appear in listings and access control.  Besides the usual
      -- database columns, they can be constructed ones as well.
      data Attribute v     :: *

      -- | The title of the given attribute.
      attributeTitle :: Attribute v -> Text
      
      -- | In listing of the object what all attributes should be
      -- listed.  This can be empty in which case the inline display
      -- of the object is used.
      listAttributes :: [Attribute v]
      listAttributes = []
      
      -- | Controls in which order the objects are listed. By default
      -- no sorting is done, i.e. it is governed by the database
      -- backend.
      listSort   :: [SelectOpt v]
      listSort = []

      -- | How many elements to be shown on a page. The default value
      -- is 20.
      objectsPerPage :: v -> Int
      objectsPerPage = const 20

{-|

Admin sites need to show objects in inline text or in listings of
entities in an human readable ways. This class captures objects that
can be shown inline on admin sites. The class might look unnecessarily
complicated; after all why not declare the member function
@'inlineDisplay'@ to just return a @'Text'@ instead of a monadic
action. The answer lies in the fact that for some objects like
database id's, it makes more sense to display the inline text of the
objects that the id points to rather than the plain id itself. This
would require hitting the database which means it cannot be a pure
Haskell function.

-}

class InlineDisplay sub master a where
      inlineDisplay :: a -> GHandler sub master Text

-- We now declare InlineDisplay instance for all the standard persist
-- values.

instance InlineDisplay sub master Text where
         inlineDisplay = return

instance InlineDisplay sub master String where
         inlineDisplay = return . pack

instance InlineDisplay sub master ByteString where
         inlineDisplay = return . pack . show

instance InlineDisplay sub master Day where
         inlineDisplay = return 
                       . pack
                       . formatTime defaultTimeLocale "%d %b, %Y" 

instance InlineDisplay sub master UTCTime where
         inlineDisplay = return
                       . pack
                       . formatTime defaultTimeLocale "%d %b, %Y %T %Z"


instance ( YesodPersist master
         , PersistEntity v
         , b ~ YesodPersistBackend master
         , m ~ GGHandler sub master IO
         , PersistBackend b m
         , InlineDisplay sub master v
         ) => InlineDisplay sub master (Key b v) where
         inlineDisplay key = do maybev <- runDB $ get key
                                maybe (return "Bad Key") inlineDisplay maybev

instance InlineDisplay sub master v
         => InlineDisplay sub master (Maybe v) where
         inlineDisplay mv = maybe (return "") inlineDisplay mv

instance Show a => InlineDisplay sub master a where
         inlineDisplay = return
                       . pack
                       . show


-- | This class captures display of attributes of an object. Like in
-- the case of inline display, displaying certain attributes of v
-- require hitting the database and hence the function is not a pure
-- haskell function.

class Administrable v
      => AttributeDisplay sub master v where
      attributeDisplay  :: Attribute v  -- ^ The attribute
                        -> v            -- ^ The value whose attribute
                                        -- is required
                        -> GHandler sub master Text

-- | Captures Yesod auth sites that admins. An admin is a user who has
-- some of the administartive previledges. A super user is an admin
-- user with all the administrative previledge. The default
-- definitions are the following:
--
--    * There are no super users.
--
--    * The only admin users are the super users
--
-- Therefore the default definitions *will not* enable the admin
-- interfaces at all. If you define 'isSuperUser' but not
-- 'isAdminUser' then only the superusers have access to the admin
-- site.

class YesodAuth master => HasAdminUser master where
      isSuperUser :: AuthId master -> GHandler sub master Bool
                  -- ^ Check whether the user is a superuser
      isAdminUser :: AuthId master -> GHandler sub master Bool
                  -- ^ Check whether the user is an admin. An
                  -- admin is a user who has access to some of
                  -- the admin facilities. All superusers are by
                  -- default admin users.
      isAdminUser   = isSuperUser
      isSuperUser _ = return False

-- | All functions in the admin handlers generate abstract admin pages
-- like admin listings, admin forms etc.  which needs to be rendered
-- as HTML/CSS. This class configures how those abstract pages are
-- rendered. The default rendering uses the combinators defined in the
-- module "Yesod.Admin.Render.Defaults" and should work for you (you
-- might want to change the branding though). On the other hand you
-- can design a new admin "skin" and redefine all the members
-- here. You can get quite a bit of configuration by just changing the
-- styles used. To do this modify `adminStyles`.

class Yesod master => HasAdminLayout master where

      -- | Sets the branding of the admin site. Being a widget you can
      -- perform arbitrary widget actions like adding styles
      -- here. However it is better to keep it simple and delegate
      -- those to other members (like adminStyles) of the class.

      branding   :: GWidget sub master ()
      branding   = do setTitle "Yesod Admin"
                      addHtml [shamlet|Yesod Admin|]
      
      -- | Sets up the styles to use on admin pages. While you can
      -- have arbitray widget code here, it is better to add only the
      -- style related stuff here. This way you can change the style
      -- without making any changes to the code what so ever.

      adminStyles :: GWidget sub master ()
      adminStyles = defaultAdminStyles
      
      -- | The layout of the admin site. This is where you do the
      -- final tweaks to the widget before you send it out to the
      -- world. The purpose of this is similar to that of
      -- `defaultLayout` in the `Yesod` class but this is restricted
      -- to only the admin pages of this site.

      adminLayout :: GWidget sub master a  -- ^ The admin widget to render
                  -> GHandler sub master RepHtml
      adminLayout content = defaultAdminLayout $ do adminStyles
                                                    [whamlet|
                                                        <div .branding>^{branding}
                                                        <div .content>^{content}
                                                    |]

      -- | Controls how to render an admin listing.
      listingToContents :: Listing master -> GWidget sub master ()
      listingToContents = defaultListing
{-|

This class captures the admin interface of the object @v@ on the site
@master@. Don't be scared of the context declaration; the master site
needs
    
    * Database access and hence YesodPersist

    * Needs a way to check for users with administrative permissions
      and hence HasAdminUser

    * Needs a way to layout admin pages and hence HasAdminLayout

The object @v@ needs an:
    
    * An admininstrative interfaces,

    * A way of inline display with the database backend matching that of 
      the master site and
    
    * A way of displaying attributes of the object.

The member function of this class specify the access control and
relevant forms. Often, the default settings is all that is needed; all
operations are allowed for super user(s) and no one else. However,
feel free to configure the appropriate access control combinator.

-}

class ( YesodPersist master
      , HasAdminUser master
      , HasAdminLayout master
      , Administrable v
      , PersistBackend (YesodPersistBackend master)
                       (GGHandler (Admin master v) master IO)
      , InlineDisplay (Admin master v) master v
      , AttributeDisplay (Admin master v) master v
      ) => YesodAdmin master v where

      -- | This filter controls what objects are displayed to a given
      -- user. Beware that restricting this filter is not a substitute
      -- to proper access control. You need to also set the `canRead`
      -- and/or `canReadAttribute` appropriately. Otherwise if a user
      -- guess the id of an object, it will be displayed. This filter
      -- is to weed out undisplayable objects from listings.
      listFilter :: AuthId master -> master -> [Filter v]
      listFilter _ _ = []

      -- | This controls whether a user is allowed to create a
      -- particular object. The default setting only allows the super
      -- user to create.
      canCreate  :: AuthId master -- ^ The user
                 -> v             -- ^ The object to create
                 -> AdminHandler master v Bool
      canCreate authId _ = isSuperUser authId

      -- | Controls whether a user can replace a given object with a
      -- new object. By default only the super user is allowed.
      canReplace :: AuthId master -- ^ The user
                 -> AdminKVPair master v -- ^ existing object
                 -> v                    -- ^ the new object
                 -> AdminHandler master v Bool
      canReplace authId _ _ = isSuperUser authId

      -- | Controls whether a particular user is allowed to delete an
      -- object. By default only the super user is allowed.
      canDelete  :: AuthId master
                 -> AdminKVPair master v
                 -> AdminHandler master v Bool
      canDelete authId _ = isSuperUser authId

      -- | Controls whether a particular user is allowed to read an
      -- object. By default only the super user is allowed. To avoid
      -- the unreadable objects from being listed, ensure that the
      -- `listFilter` weeds them out.
      canRead :: AuthId master
              -> AdminKVPair master v
              -> AdminHandler master v Bool
      canRead authId _ = isSuperUser authId

      -- | Controls whether a particular user is allowed to read a
      -- particular attribute of the object. Notice that to read a
      -- attribute, one needs the read permission any way. By default
      -- only the super user is allowed.
      canReadAttribute  :: AuthId master
                        -> AdminKVPair master v
                        -> Attribute v     -- ^ Given attribute
                        -> AdminHandler master v Bool
      canReadAttribute authId _ _ = isSuperUser authId
