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
       , defaultAttributeDisplay
       ) where

import qualified Data.Text as T
import Text.Hamlet
import Text.Cassius
import Yesod
import Yesod.Admin.Render
import Yesod.Admin.Message

type Text = T.Text


mkZebraAttrs :: [(a,b)] -> [(Text,a,b)]
mkZebraAttrs = zipWith f $ cycle ["odd","even"]
    where f z (a,b) = (z,a,b)

renderButton :: ( RenderRoute master
                , RenderMessage master AdminMessage
                ) => Button master -> GWidget sub master ()
renderButton button = $(whamletFile "templates/render/default/button.hamlet")
         where link = buttonLink button
               name = buttonName button
               clzs = buttonClass button

defaultAttributeDisplay :: ( RenderRoute master
                           , RenderMessage master (AdminMessage)
                           , RenderMessage master t
                           , RenderMessage master attr
                           )
                           => AttributeDisplay master t attr
                           -> GWidget sub master ()

defaultAttributeDisplay dsp =
        $(whamletFile "templates/render/default/attribute-display.hamlet")
      where title      = displayTitle dsp
            zebraAttrs = mkZebraAttrs $ displayAttrList dsp
            buttons    = map renderButton $ controlButtons dsp

defaultListing :: Listing master -> GWidget sub master ()
defaultListing lst = addWidget [whamlet|
                        <div .pagetitle>  #{listingSingular lst}
                        <div .listing .shadow>
                           <table .admin>
                              <tr .header>
                                  $forall h <- listingHeaders lst
                                     <th> #{h}
                              $forall (parity,r) <- taggedRows
                                  <tr .highlight .#{parity}>
                                      ^{r}
                           <div .listing-footer>
                                #{totalObjects lst} #{objects}
                     |]
      where taggedRows = zip (cycle ["odd" :: Text, "even"]) tableRows
            tableRows  = map link $ listingRows lst
            link (route, t:ts) = sequence_  (mkLink route t : map mkText ts)
            link (_, [])       = return ()
            objects = if totalObjects lst > 1 then listingPlural lst
                      else listingSingular lst

mkLink route t = toWidget [hamlet|
                    <td>
                        <a href=@{route}> #{t}
                    |]


mkText t = toWidget [shamlet| <td> #{t} |]

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
defaultAdminStyles = do toWidget adminStyle
                        toWidget tableStyle
                        toWidget buttonStyle

-- | Colours of the main panel
mainPanelBackground :: Text
mainPanelText       :: Text
mainPanelPadding    :: Text
mainPanelPaddingSides :: Text
mainPanelBackground  = "black" :: Text
mainPanelText        = "white" :: Text
mainPanelPadding      = "1em" :: Text
mainPanelPaddingSides = "0.25em" :: Text

-- | Panel properties
panelBackground :: Text
panelTextColour :: Text
panelBackground     = "#DDDDDD" :: Text
panelTextColour     = "black" :: Text


-- | General  Border properties
borderColour :: Text
borderWidth  :: Text
borderColour     = "DarkGrey" :: Text
borderWidth      = "thin"    :: Text

-- | Borders of hilighted entries
highlightWidth :: Text
highlightColour :: Text
highlightWidth   = "thin"   :: Text
highlightColour  = "black"   :: Text


penColour :: Text
penColour = "black"     :: Text

-- | Colour of links.
linkColour :: Text
linkColour  = "DarkCyan"  :: Text

-- Colour and width of shadow objects.
shadowColour :: Text
shadowWidth :: Text
shadowColour    = "#999999" :: Text
shadowWidth     = "10px"    :: Text

-- | Alternating colours of a zebra table
zebraOddColour  :: Text
zebraEvenColour :: Text
zebraOddColour  = "white"      :: Text
zebraEvenColour = "#F0F0F0"  :: Text

vspace :: Text
hspace :: Text
vspace          = "0.25em"   :: Text
hspace          = "0.5em"    :: Text

-- | The style files to include for the default admin
adminStyle  = $(cassiusFile "templates/admin.cassius")
tableStyle  = $(cassiusFile "templates/table.cassius")
buttonStyle = $(cassiusFile "templates/button.cassius")
