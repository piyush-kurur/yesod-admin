{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-|

Admin pages have contents that are listings of object, forms to edit
objects, pages asking for confirmation of certain actions etc. This
module defines classes and datatypes to abstract out this rendering
and layout issues. This provides for flexible theming of admin sites.

-}
module Yesod.Admin.Render
       ( Button (..)
       , Listing(..)
       , HasAdminRendering(..)
       ) where

import Data.Text(Text)
import Text.Hamlet
import Yesod

import Yesod.Admin.Message
import Yesod.Admin.Class
import Yesod.Admin.Render.Default

-- | The rendering of admin pages of a master site is controlled by
-- the following type class. The default rendering uses the
-- combinators defined in the module "Yesod.Admin.Render.Defaults" and
-- should work for you (you might want to change the branding
-- though). On the other hand you can design a new admin "skin" and
-- redefine all the members here. You can get quite a bit of
-- configuration by just changing the styles used. To do this modify
-- `adminStyles`.

class Yesod master => HasAdminRendering master where

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
      
      -- | How to show a read page
      adminReadPage :: ( Administrable v
                       , RenderMessage master AdminMessage
                       , RenderMessage master (Attribute v)
                       , RenderMessage master (Collective c)
                       )
                    => [(Attribute v, Text)]
                    -> Route master -- ^ route to create
                    -> Route master -- ^ route to update
                    -> Route master -- ^ route to delete
                    -> GWidget sub master ()
                       
      adminDeletePage :: ( Administrable v
                         , RenderMessage master AdminMessage
                         , RenderMessage master (Attribute v)
                         , RenderMessage master (Collective c)
                         ) 
                      => [(Attribute v, Text)]
                      -> GWidget sub master ()
      
                       
                       
                       
      
      -- | The layout of the admin site. This is where you do the
      -- final tweaks to the widget before you send it out to the
      -- world. The purpose of this is similar to that of
      -- `defaultLayout` in the `Yesod` class but this is restricted
      -- to only the admin pages of this site.

      adminLayout :: GWidget sub master a  -- ^ The admin widget to render
                  -> GHandler sub master RepHtml
      adminLayout content
                  = defaultAdminLayout
                            $  adminStyles
                            >> [whamlet|
                                    <div .branding> ^{branding}
                                    <div .toolbar>  ^{toolBar}
                                    <div .content>  ^{content'}
                               |]
           where content' = content >> return ()



-- | A button.
data Button master = Button { buttonLink  :: Route master
                            , buttonName  :: AdminMessage
                            , buttonClass :: [Text]
                            }

-- | Admin pages show the listing of objects. This is the data type
-- that controls it.

data Listing master = Listing { listingSingular     :: Text
                                      -- ^ Singular name
                              , listingPlural       :: Text
                                      -- ^ Plural name
                              , listingHeaders  :: [Text]
                                      -- ^ The headers of the listing
                              , listingRows     :: [(Route master, [Text])]
                                      -- ^ The actual list. The route
                                      -- to the object is provided to
                                      -- facilitate linking.
                              , pageNumber      :: Int
                                      -- ^ Current page.
                              , perPageCount    :: Int
                                      -- ^ Total objects per page.
                              , totalObjects    :: Int
                                      -- ^ Total number of objects.
                              }
