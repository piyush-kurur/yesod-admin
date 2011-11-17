{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}

{-|

Default rendering of primitives.

-}

module Yesod.Admin.Render.Defaults
       ( defaultListing
       , defaultAdminLayout
       , defaultAdminStyles
       ) where

import Text.Blaze
import Data.Text(Text, toLower)
import Text.Hamlet
import Text.Cassius
import Yesod
import Yesod.Admin.Render

defaultListing :: Listing master -> GWidget sub master ()
defaultListing lst = addWidget [whamlet|
                        <h2 .objectname> #{listingName lst}
                        <div .listing .shadow>
                           <table .admin>
                              <tr .header>
                                  $forall h <- listingHeaders lst
                                     <th> #{h}
                              $forall (parity,r) <- taggedRows
                                  <tr .highlight .#{parity}>
                                      ^{r}
                     |]
      where taggedRows = zip (cycle ["odd" :: Text, "even"]) tableRows
            tableRows  = map link $ listingRows lst
            link (route, t:ts) = sequence_  (mkLink route t : map mkText ts)


mkLink route t = addHamlet [hamlet| 
                <td>
                    <a href=@{route}> #{t}
                |]
  

mkText t = addHtml [shamlet| <td> #{t} |]

-- | The layout to use for admin pages.
defaultAdminLayout :: Yesod master 
            => GWidget sub master ()
            -> GHandler sub master RepHtml

defaultAdminLayout w = do p <- widgetToPageContent w
                          hamletToRepHtml [hamlet|
                             !!!
                             <html>
                                <head>
                                        <title>#{pageTitle p}
                                        ^{pageHead p}
                                <body>
                                        ^{pageBody p}
                             |]


defaultAdminStyles :: GWidget sub master ()
defaultAdminStyles = do addCassius adminStyle
                        addCassius tableStyle
                        addCassius buttonStyle


penColour       = "steelblue"    :: Text

shadowColour    = "#999999" :: Text
shadowWidth     = "10px"    :: Text

borderColour    = "#AAAAAA" :: Text
borderWidth     = "1px"     :: Text

panelBackground     = "#EEEEEE" :: Text
panelColour         = "#666666" :: Text


zebraOddColour  = "#E0E0FF"  :: Text
zebraEvenColour = "#FFFFF0"  :: Text

vspace          = "0.25em"   :: Text
hspace          = "0.5em"    :: Text

-- | The style files to include for the default admin
adminStyle  = $(cassiusFile "templates/admin.cassius")
tableStyle  = $(cassiusFile "templates/table.cassius")
buttonStyle = $(cassiusFile "templates/button.cassius")
