{-|

This module describes the crud interface to persistent objects.

-}

module Yesod.Admin.Crud
       ( ListingStyle(..)
       ) where

import Data.Text (Text)

{-|

Datatype that captures how objects are listed. Each object is
displayed as a column of Text objects. Each of the column has a header
which says what the field is. For example when listing say Unix users
one would probably list , for each user, her user name and actual
name. In this case the headers are 'User Name' and 'Name' say.

-}

data ListingStyle v = ListingStyle 
     { listingHeaders :: [Text] -- ^ The headers of the column
     , listingColumn  :: (v -> Text -> Text)
                      -- ^ How to obtain a particular column of an
                      -- object.
     }
