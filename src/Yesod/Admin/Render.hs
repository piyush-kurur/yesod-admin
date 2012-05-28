{-# LANGUAGE QuasiQuotes        #-}

{-|

Admin pages have contents that are listings of object, forms to edit
objects, pages asking for confirmation of certain actions etc. This
module captures data types that abstract out these pages.  The actual
rendering of the pages into HTML/CSS is done else where (see the class
`HasAdminLayout` of "Yesod.Admin.Class"). The advantage of this split
is that one can change the layout without making any change to the
rest of the admin code.

-}
module Yesod.Admin.Render
       ( Button (..)
       , AttributeDisplay(..)
       , Listing(..)
       ) where

import Data.Text(Text)
import Yesod
import Yesod.Admin.Message

-- | A button.
data Button master = Button { buttonLink  :: Route master
                            , buttonName  :: AdminMessage
                            , buttonClass :: [Text]
                            }
-- | Displaying an object.

data AttributeDisplay master t attr
              = AttributeDisplay { displayTitle     :: t
                                 , displayAttrList  :: [(attr, Text)]
                                 , controlButtons   :: [Button master]
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
