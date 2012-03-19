{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-|

This module defines the handler for the /page/ route.

-}
module Yesod.Admin.Handlers.Internal.PageR
       ( getPageR
       ) where

import Data.Text(Text)
import Yesod
import Yesod.Auth
import Yesod.Admin.Types
import Yesod.Admin.Class
import Yesod.Admin.Render
import Yesod.Admin.Handlers.Internal.Helpers
import Text.Hamlet


{-

Developers notes
----------------

The code for this handler is likely to be the most complicated as its
output depend on a lot of parameters. As more features are added the
mess is only going to increase. It makes sense to fanatically refactor
the code here.

Most of it is straight forward modulo the fact that some combinators
need explicit types to fix type ambiguities. There are lots of tedious
details that need to be computed to perform the listing. Besides that
there is nothing great that is happening here.


-}
getPageR :: ( Yesod master
            , YesodPersist master
            , b ~ YesodPersistBackend master
            , m ~ AdminHandler master v
            , PersistQuery b m
            )
          => Int   -- ^ The page to view
          -> AdminHandler master v RepHtml

getPageR pg = defaultLayout $ do
              addHamlet [hamlet| page #{pg} should be displayed |]

{-
getAdminPageR pg = withAdminUser $ getAdminPageR' pg
getAdminPageR' ::  YesodAdmin master v
               => Int   -- ^ The page to view
               -> AuthId master
               -> AdminHandler master v RepHtml

getAdminPageR' pg aid = do flt          <- fmap (listFilter aid) getYesod
                           objsPerPage  <- getObjectParams objectsPerPage
                           lstSing      <- getObjectParams objectSingular
                           lstPlur      <- getObjectParams objectPlural
                           cols         <- getObjectAttributes
                           objCount     <- runDB $ count flt
                           rows         <- getRows aid pg objsPerPage flt listSort
                           adminLayout $ listingToContents $ 
                               Listing { listingSingular = lstSing
                                       , listingPlural   = lstPlur
                                       , perPageCount    = objsPerPage
                                       , pageNumber      = pg
                                       , totalObjects    = objCount
                                       , listingHeaders  = map attributeTitle cols
                                       , listingRows     = rows
                                       }


getRows :: YesodAdmin master v
        => AuthId master
        -> Int               -- ^ Page number
        -> Int               -- ^ object per page
        -> [Filter v]        -- ^ filters to apply
        -> [SelectOpt v]     -- ^ Sorting options
        -> AdminHandler master v [(Route master, [Text])] 

getRows aid p n fltr sort = do liftR <- getRouteToMaster
                               kvps  <- getObjects p n fltr sort
                               getRow aid liftR kvps

getRow :: YesodAdmin master v
       => AuthId master
       -> (Route (Admin master v) -> Route master)
       -> [AdminKVPair master v]
       -> AdminHandler master v [(Route master, [Text])]
getRow aid liftR = fmap (filter readable) . sequence . map (mapper attrs)
       where attrs = listAttributes
             mapper ats kvp@(k,_) = do r <- getAttributes aid kvp ats
                                       return (liftR $ AdminReadR k, r)
             readable (_,x) = not $ null x



getObjects :: YesodAdmin master v
           => Int               -- ^ Page number
           -> Int               -- ^ object per page
           -> [Filter v]        -- ^ filters to apply
           -> [SelectOpt v]     -- ^ Sorting options
           -> AdminHandler master v [AdminKVPair master v]
getObjects p n fltr sort = runDB $ selectList fltr opts
           where opts   = sort ++ [OffsetBy start, LimitTo n]
                 start  = p * n


getAttribute :: YesodAdmin master v
             => AuthId master
             -> AdminKVPair master v
             -> Attribute v
             -> AdminHandler master v Text
getAttribute aid kvp@(_,v) at
             = do perm <- canReadAttribute aid kvp at
                  if perm then attributeDisplay at v
                          else return ""

getAttributes :: YesodAdmin master v
              => AuthId master
              -> AdminKVPair master v
              -> [Attribute v]
              -> AdminHandler master v [Text]

getAttributes aid kvp ats
              = do perm <- canRead aid kvp
                   if perm then sequence $ map (getAttribute aid kvp) ats
                      else return []

{-

Developer notes.
---------------


The definition of the next few functions look pointless however they
are used to silence the type checker. For example using listAttributes
directly in expressions would not work because of ambiguity of types.
The explicit handler types fixes these ambiguities. 

These functions are candidate for refactoring to the
Y.A.Handler.Helpers module if they are found useful in other handlers.

-}

getObjectParams :: YesodAdmin master v
                => (v -> a)
                -> AdminHandler master v a
getObjectParams f = return $ f undefined

getObjectAttributes :: YesodAdmin master v
                    => AdminHandler master v [Attribute v]
getObjectAttributes = return listAttributes

-}