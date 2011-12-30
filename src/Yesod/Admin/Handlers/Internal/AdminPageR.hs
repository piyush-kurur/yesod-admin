{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}

{-|

This module defines the handler for the /page/ route.

-}
module Yesod.Admin.Handlers.Internal.AdminPageR
       ( getAdminPageR
       ) where

import Data.Text(Text)
import Yesod
import Yesod.Auth
import Yesod.Admin.Subsite
import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.Render
import Yesod.Admin.Handlers.Helpers


{-

Developers notes
----------------

The code for this handler is likely to be the most complicated as its
output depend on a lot of parameters. As more features are added the
mess is only goint to increase. It makes sense to fanatically refactor
the code here. 

Most of it is straight forward modulo the fact that some combinatorts
need explicit types to fix type ambiguities. There are lots of tedious
details that need to be computed to perform the listing. Besides that
there is nothing great that is happening here.


-}
getAdminPageR ::  YesodAdmin master v
              => Int   -- ^ The page to view
              -> AdminHandler master v RepHtml

getAdminPageR pg = withAdminUser $ getAdminPageR' pg
getAdminPageR' ::  YesodAdmin master v
               => Int   -- ^ The page to view
               -> AuthId master
               -> AdminHandler master v RepHtml

getAdminPageR' pg aid = do flt          <- fmap (listFilter aid) getYesod
                           objsPerPage  <- getObjectParams objectsPerPage
                           lstSing      <- getObjectParams objectSingular
                           lstPlur      <- getObjectParams objectPlural
                           liftR        <- getRouteToMaster
                           cols         <- getObjectColumns
                           objCount     <- runDB $ count flt
                           rows         <- getRows aid pg objsPerPage flt listSort
                           adminLayout $ listingToContents $ 
                               Listing { listingSingular = lstSing
                                       , listingPlural   = lstPlur
                                       , perPageCount    = objsPerPage
                                       , pageNumber      = pg
                                       , totalObjects    = objCount
                                       , listingHeaders  = map columnTitle cols
                                       , listingRows     = rows
                                       }


getRows :: YesodAdmin master v
        => AuthId master
        -> Int               -- ^ Page number
        -> Int               -- ^ object per page
        -> [Filter v]        -- ^ filters to apply
        -> [SelectOpt v]     -- ^ Sorting options
        -> AdminHandler master v [(Route master, [Text])] 

getRows aid p n filter sort = do liftR <- getRouteToMaster
                                 kvps  <- getObjects p n filter sort
                                 getRow aid liftR kvps

getRow :: YesodAdmin master v
       => AuthId master
       -> (Route (Admin master v) -> Route master)
       -> [AdminKVPair master v]
       -> AdminHandler master v [(Route master, [Text])]
getRow aid liftR = fmap (filter readable) . sequence . map (mapper cols)
       where cols = listColumns
             mapper cols kvp@(k,v) = do r <- getColumns aid kvp cols
                                        return (liftR $ AdminReadR k, r)
             readable (_,x) = not $ null x



getObjects :: YesodAdmin master v
           => Int               -- ^ Page number
           -> Int               -- ^ object per page
           -> [Filter v]        -- ^ filters to apply
           -> [SelectOpt v]     -- ^ Sorting options
           -> AdminHandler master v [AdminKVPair master v]
getObjects p n filter sort = runDB $ selectList filter opts
           where opts   = sort ++ [OffsetBy start, LimitTo n]
                 start  = p * n


getColumn :: YesodAdmin master v
          => AuthId master
          -> AdminKVPair master v
          -> Column v
          -> AdminHandler master v Text
getColumn aid kvp@(k,v) col = do perm <- canReadColumn aid kvp col
                                 if perm then runDB $ columnDisplay col v
                                    else return ""

getColumns :: YesodAdmin master v
           => AuthId master
           -> AdminKVPair master v
           -> [Column v]
           -> AdminHandler master v [Text]

getColumns aid kvp cols = do perm <- canRead aid kvp
                             if perm then sequence $ map (getColumn aid kvp) cols
                                else return []

{-

Developer notes.
---------------


The definition of the next few functions look pointless however they
are used to silence the type checker. For example using listColumns
directly in expressions would not work because of ambiguity of types.
The explicit handler types fixes these ambiguities. 

These functions are candidate for refactoring to the
Y.A.Handler.Helpers module if they are found useful in other handlers.

-}

getObjectParams :: YesodAdmin master v
                => (v -> a)
                -> AdminHandler master v a
getObjectParams f = return $ f undefined

getObjectColumns :: YesodAdmin master v
                 => AdminHandler master v [Column v]
getObjectColumns = return listColumns
