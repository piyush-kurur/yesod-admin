{-|

This module describes the crud interface to persistent objects.

-}

module Yesod.Admin.Crud
       ( ListingStyle(..)
       , CRUD(..)
       ) where

import Yesod
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

{-|

An admin site exposes different crud operations to different users.
This data type parameterises various crud operations.

-}

data CRUD sub master v = CRUD
     { listingStyle  :: ListingStyle v
                     -- ^ How to list it.
     , listingFilter :: [Filter v]
                     -- ^ List only those matching this filter.
     , listingSort   :: [SelectOpt v]
                     -- ^ How to sort the objects.
     , listingSize   :: Maybe Int
                      -- ^ Bound on maximum objects to list.
     , createForm    :: Form sub master v
                     -- ^ Form used for creation.
     , updateForm    :: v -> Form sub master v
                     -- ^ Form used for modification.
     }

-- TOTHINK: How should one display values. Tricky part is objects which
-- have ids in it. Clearly displaying the ids are no fun.
