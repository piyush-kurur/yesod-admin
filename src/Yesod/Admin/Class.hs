{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE QuasiQuotes               #-}


{-|

Sites with admin interfaces. 

-}
module Yesod.Admin.Class
       ( Administrable(..)
       , InlineDisplay(..)
       , ColumnDisplay(..)
       , HasSuperUser (..)
       , HasAdminLayout (..)
       , YesodAdmin(..)
       ) where

import Data.Text(Text, append, pack)
import Yesod
import Yesod.Auth
import Database.Persist.Base
import Yesod.Admin.Helpers
import Yesod.Admin.Subsite
import Yesod.Admin.Types
import Yesod.Admin.Render
import Yesod.Admin.Render.Defaults
import Text.Hamlet

-- | This class captures objects that have an admin
-- interfaces. Minimum complete definition includes the data type
-- @'Column' v@ and the function 'columnTitle'.

class PersistEntity v => Administrable v where

      -- | The name of the object. Used in various places for example
      -- in titles of admin pages. The default values is the entity
      -- of the given persistent type.
      objectSingular :: v -> Text
      objectSingular = pack . unCamelCase . entityName . entityDef

      -- | The plural form of the object. By default an `s' is
      -- appended to the singular form.
      objectPlural  :: v -> Text
      objectPlural  v = objectSingular v `append` "s"

      -- | Abstract columns of the type. These columns can appear in
      -- listings and access control.  Besides the usual database
      -- columns they may capture other constructed columns.
      data Column v     :: *

      -- | The title of the given column.
      columnTitle :: Column v -> Text
      
      -- | In listing of the object what all columns should be listed.
      -- This can be empty in which case the inline display of the
      -- object is used.
      listColumns :: [Column v]
      listColumns = []
      
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
complicates; after all why not declare the member function
`inlineDisplay` to just return a Text instead of a monadic action. The
answer lies in the fact that for some objects like database id's, it
makes more sense to display the inline text of the objects that the id
points to rather than the plain id itself. This would require hitting
the database which means it cannot be a pure Haskell function.

-}

class PersistBackend b m => InlineDisplay b m a where
      inlineDisplay :: a -> b m Text

instance PersistBackend b m => InlineDisplay b m Text where
         inlineDisplay = return

instance PersistBackend b m => InlineDisplay b m String where
         inlineDisplay = return . pack

instance (Show a, PersistBackend b m) => InlineDisplay b m a where
         inlineDisplay = return . pack . show

instance ( PersistEntity v
         , PersistBackend b m
         , InlineDisplay b m v
         ) => InlineDisplay b m (Key b v) where
        
         inlineDisplay key = do maybev <- get key
                                maybe (return "Bad Key") inlineDisplay maybev

-- | This class captures display of columns of an object. Like in the
-- case of inline display, displaying certain columns of v require
-- hitting the database and hence the function is not a pure haskell
-- function.

class (PersistBackend b m, Administrable v) => ColumnDisplay b m v where
      columnDisplay     :: Column v  -- ^ The column
                        -> v         -- ^ The value whose column is required
                        -> b m Text

-- | Captures Yesod auth sites that have a super user.
class YesodAuth master => HasSuperUser master where
      isSuperUser :: AuthId master -> GHandler sub master Bool

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
      -- perform arbitrary widget actions like adding styles here. However
      -- it is better to keep it simple and delegate those to other members
      -- (like adminStyles) of the class.

      branding   :: GWidget sub master ()
      branding   = do setTitle "Yesod Admin"
                      addHtml [shamlet|Yesod Admin|]
      
      -- | Sets up the styles to use on admin pages. While you can have
      -- arbitray widget code here, it is better to add only the style
      -- related stuff here. This way you can change the style without
      -- making any changes to the code what so ever.

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
@master@. Don't be scared of the context declaration; the master site needs
    
    * Database access and hence YesodPersist

    * Needs a super user and hence HasSuperUser

    * Needs a way to layout admin pages and hence HasAdminLayout

The object @v@ needs an:
    
    * An admininstrative interfaces,

    * A way of inline display with the database backend matching that of 
      the master site and
    
    * A way of displaying columns of the object.

The member function of this class specify the access control and
relevant forms. Often, the default settings is all that is needed; all
operations are allowed for super user(s) and no one else. However,
feel free to configure the appropriate access control combinator.

-}

class ( YesodPersist master
      , HasSuperUser master
      , HasAdminLayout master
      , Administrable v
      , InlineDisplay (YesodPersistBackend master)
                      (GGHandler (Admin master v) master IO)
                      v
      , ColumnDisplay (YesodPersistBackend master)
                      (GGHandler (Admin master v) master IO)
                      v
      ) => YesodAdmin master v where

      -- | This filter controls what objects are displayed to a given
      -- user. Beware that restricting this filter is not a substitute
      -- to proper access control. You need to also set the `canRead`
      -- and/or `canReadColumn` appropriately. Otherwise if a user
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
      -- particular column of the object. Notice that to read a
      -- column, one needs the read permission any way. By default
      -- only the super user is allowed.
      canReadColumn  :: AuthId master
                     -> AdminKVPair master v
                     -> Column v     -- ^ Given column
                     -> AdminHandler master v Bool
      canReadColumn authId _ _ = isSuperUser authId
