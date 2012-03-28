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

The master site on which we want an admin interfaces should be an
instance of

* `YesodPersist` for otherwise what will we administer

* `HasAdminUser` so that we can define proper access control and

* `HasAdminLayout` so that we can change the look and feel of the admin site.

Any persistent entity that we want to administer should be an instance
of the following classes.


* The class 'Administrable'. This class is used to define things like
  name of the object the attributes in listings of the object etc.

* The class 'InlineDisplay'. This class controls how a particular
  object is showed in inline text.

* The class 'AttributeDisplay'. This class controls what is shown in
  the listing of a particular object.

Finally what admin operations are possible for an entity on a
particular site depends on the persistent backend of that site. For
basic CRUD operations one needs the persistent backend to be an
instance of `PersistStore`. If this is satisfied one can define an
instance for the class `CrudControl` for that site entity pair. If
in addition the backend supports selection, one can define mass
actions like mass deletion from a selection list. In such a case the
site entity pair has to be an instance of `SelectionControl`.

-}
module Yesod.Admin.Class
       (
       -- * Entity classes.
         Administrable(..)
       , InlineDisplay(..)
       , AttributeDisplay(..)
       , HasAdminForms(..)
       -- * Permissions and Access control.
       , HasAdminUsers (..)
       , CrudControl (..)
       , SelectionControl (..)
       -- * Controlling layout and style
       , HasAdminLayout (..)
       , LiftCrudRoutes (..)
       ) where

import Data.ByteString (ByteString)
import Data.Text(Text, append, pack)
import System.Locale
import Data.Time
import Database.Persist.EntityDef
import Yesod
import Yesod.Form
import Yesod.Auth
import Yesod.Admin.Helpers.Text
import Yesod.Admin.Types
import Yesod.Admin.Render.Defaults
import Text.Hamlet

{-|

    This class captures objects that have an admin interface. The most
important member of this class is the associated data type of this
class are @'Attribute'@ and @'Action'@. An attribute of an object can
either be a database column of an object or might be a constructed
entity. The former we call a /database attribute/ whereas the later we
call a /derived attribute/. Each attribute also has a title which is
used in the selection list of the object. The member function
@'attributeTitle'@ maps an attribute of a data type @`v`@ (of type
@'Attribute' v@) to its title.

Minimum complete definition of this class requires defining the
associated types @'Attribute' v@ and @'Action' v@, the variable
@'dbAttributes'@, @'attributeTitle'@ and @'dbAction'@


-}

class ( Eq (Attribute v)
      , Enum (Attribute v)
      , Bounded (Attribute v)
      , PersistEntity v
      )
      => Administrable v where

      -- | The name of the object. Used in various places for example
      -- in titles of admin pages. The default values is the entity
      -- name of the given persistent type.
      objectSingular :: v -> Text
      objectSingular = unCamelCase
                     . unHaskellName
                     . entityHaskell
                     . entityDef

      -- | The plural form of the object. By default an `s' is
      -- appended to the singular form.
      objectPlural  :: v -> Text
      objectPlural  v = objectSingular v `append` "s"

      -- | Abstract attributes of the type. These attributes can
      -- appear in listings and access control. Besides the usual
      -- database columns, they can be constructed ones as well.
      data Attribute v :: *

      -- | Admin actions on this object.
      data Action v    :: *

      -- | The title of the given attribute.
      attributeTitle :: Attribute v -> Text

      -- | The db action associate with and admin action.
      dbAction :: PersistStore b m => Action v -> DBAction b m v

      -- | The database attributes of the object.
      dbAttributes :: [Attribute v]
      -- | In the selection page of the object what all attributes
      -- should be listed. This can be empty in which case the inline
      -- display of the object is used.
      selectionPageAttributes :: [Attribute v]
      selectionPageAttributes = []

      -- | In the read page of an object what attributes should be shown.
      readPageAttributes :: [Attribute v]
      readPageAttributes = dbAttributes

      -- | Controls in which order the objects are listed on the
      -- selection page. By default no sorting is done, i.e. it is
      -- governed by the database backend.
      selectionPageSort   :: [SelectOpt v]
      selectionPageSort = []

      -- | How many elements to be shown on a page. The default value
      -- is 20.
      objectsPerPage :: v -> Int
      objectsPerPage = const 20


-- | The crud forms of the entity.
class PersistEntity v => HasAdminForms v where
      creatForm  :: MForm sub master v
      updateForm :: SiteKVPair master v -> MForm sub master v

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

class (Monad m, PersistStore b m)
      => InlineDisplay b m a where
      inlineDisplay :: a -> b m Text

-- We now declare InlineDisplay instance for all the standard persist
-- values.

instance (Monad m, PersistStore b m) => InlineDisplay b m Text where
         inlineDisplay = return

instance (Monad m, PersistStore b m) => InlineDisplay b m String where
         inlineDisplay = return . pack

instance (Monad m, PersistStore b m) =>
         InlineDisplay b m ByteString where
         inlineDisplay = return . pack . show

instance (Monad m, PersistStore b m) => InlineDisplay b m Day where
         inlineDisplay = return
                       . pack
                       . formatTime defaultTimeLocale "%d %b, %Y"

instance (Monad m, PersistStore b m) => InlineDisplay b m UTCTime where
         inlineDisplay = return
                       . pack
                       . formatTime defaultTimeLocale "%d %b, %Y %T %Z"


instance ( Monad m
         , PersistStore b m
         , InlineDisplay b m v
         , PersistEntity v
         ) => InlineDisplay b m (Key b v) where
         inlineDisplay key = do maybev <- get key
                                maybe (return "Bad Key") inlineDisplay maybev


instance ( Monad m
         , PersistStore b m
         , InlineDisplay b m v
         )
         => InlineDisplay b m (Maybe v) where
         inlineDisplay mv = maybe (return "") inlineDisplay mv

instance ( Monad m
         , PersistStore b m
         , Show a
         )
         => InlineDisplay b m a where
         inlineDisplay = return
                       . pack
                       . show


-- | This class captures display of attributes of an object. Like in
-- the case of inline display, displaying certain attributes of v
-- require hitting the database and hence the function is not a pure
-- haskell function.

class ( Monad m
      , PersistStore b m
      , Administrable v
      )
      => AttributeDisplay b m v where
      attributeDisplay  :: Attribute v  -- ^ The attribute
                        -> v            -- ^ The value whose attribute
                                        -- is required
                        -> b m Text

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

class ( YesodAuth master
      , YesodPersist master
      ) => HasAdminUsers master where
      isSuperUser :: Monad (YesodDB sub master)
                  => AuthId master -> YesodDB sub master Bool
                  -- ^ Check whether the user is a superuser
      isAdminUser :: Monad (YesodDB sub master)
                  => AuthId master -> YesodDB sub master Bool
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
      branding   = do toWidget [shamlet|Yesod Admin|]

      -- | Toolbar for your admin site. This is where you might want
      -- to include a welcome message and/or login, logout links.

      toolBar    :: GWidget sub master ()
      toolBar    = return ()

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
      adminLayout content = defaultAdminLayout
                          $ do _ <- adminStyles
                               [whamlet|
                                    <div .branding>^{branding}
                                    <div .toolbar>^{toolBar}
                                    <div .content>^{content}
                               |]

-- | The class defines the access control for crud operations.
class ( YesodPersist master
      , HasAdminUsers master
      , Administrable v
      ) => CrudControl master v where

      -- | This controls whether a user is allowed to create a
      -- particular object. The default setting only allows the super
      -- user to create.
      canCreate  :: Monad (YesodDB sub master)
                 => AuthId master -- ^ The user
                 -> v             -- ^ The object to create
                 -> YesodDB sub master Bool
      canCreate authId _ = isSuperUser authId

      -- | Controls whether a particular user can read a given
      -- object.
      canRead :: Monad (YesodDB sub master)
              => AuthId master
              -> SiteKey master v
              -> YesodDB sub master Bool
      canRead authId _ = isSuperUser authId

      -- | The attributes that can be read.
      readableAttributes :: Monad (YesodDB sub master)
                         => AuthId master
                         -> YesodDB sub master [Attribute v]
      readableAttributes authId = do issup <- isSuperUser authId
                                     if issup then return [minBound..maxBound]
                                        else return []
      -- | Controls whether a particular user is allowed to delete an
      -- object. By default only the super user is allowed.
      canDelete  :: Monad (YesodDB sub master)
                 => AuthId master
                 -> SiteKVPair master v
                 -> YesodDB sub master Bool
      canDelete authId _ = isSuperUser authId

      -- | Controls whether a user can replace a given object with a
      -- new object. By default a user can replace an existing one if
      -- she can delete the current value and insert the new one.
      canReplace :: Monad (YesodDB sub master)
                 => AuthId master -- ^ The user
                 -> SiteKVPair master v -- ^ existing object
                 -> v                   -- ^ the new object
                 -> YesodDB sub master Bool
      canReplace authId kv v = do delPerm    <- canDelete authId kv
                                  createPerm <- canCreate authId v
                                  return (delPerm && createPerm)

-- | The class defines the access control for selection operations. We
-- use filters to check access permissions on a selected list of
-- objects. Note that one can support selection pages only if the
-- underlying backed for the site is an instances of @`PersistQuery`@.

class ( YesodPersist master
      , HasAdminUsers master
      , Administrable v
      ) => SelectionControl master v where

      readFilter :: Monad (YesodDB sub master)
                 => AuthId master
                 -> YesodDB sub master (Filter v)
      readFilter authId = do issup <- isSuperUser authId
                             if issup then return $ FilterAnd []
                                else return $ FilterOr []

      deleteFilter :: Monad (YesodDB sub master)
                   => AuthId master
                   -> YesodDB sub master (Filter v)
      deleteFilter authId = do issup <- isSuperUser authId
                               if issup then return $ FilterAnd []
                                  else return $ FilterOr []

      -- ^ Filters objects on which the given action is allowed.
      actionFilter :: Monad (YesodDB sub master)
                   => AuthId master     -- ^ The admin user
                   -> Text              -- ^ Action name
                   -> YesodDB sub master (Filter v)
      actionFilter authId _ = do issup <- isSuperUser authId
                                 if issup then return $ FilterAnd []
                                    else return $ FilterOr []

-- | An internal class used to lift crud routes to master routes.
-- This instance is required so that the pages on the selection sites
-- can refer to urls on the crud subsite.

class LiftCrudRoutes master v where
      liftCrudR :: Route (Crud master v) -> Route master
