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
       ( Listing(..)
       ) where

import Data.Text(Text)
import Yesod
import Yesod.Admin.Types

-- | Admin pages show the listing of objects. This is the data type
-- that controls it.

data Listing master = Listing { listingName     :: Text 
                                      -- ^ Name of the object listed.
                              , listingHeaders  :: [Text]
                                      -- ^ The headers of the listing
                              , listingRows     :: [(Route master, [Text])]
                                      -- ^ The actual list. The id's
                                      -- of objects are provied to
                                      -- facilitate generating links.
                              , pageNumber      :: Int
                                      -- ^ Current page.
                              , perPageCount    :: Int
                                      -- ^ Total objects per page.
                              , totalObjects    :: Int
                                      -- ^ Total number of objects.
                              }
