{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

{-

Declares the yesod dispatch instance for the admin subsite.


-}

module Yesod.Admin.Dispatch where

import Data.Text(Text)

import Yesod
import Yesod.Admin.Subsite
import Yesod.Admin.Handlers
import Yesod.Admin.Class
import Yesod.Admin.Types

instance ( Yesod master
         , YesodAdmin master v
         , YesodPersist master
         , b ~ YesodPersistBackend master
         , m ~ GGHandler (Admin master v) master IO
         , PersistEntity v
         , PersistBackend b m
         , SinglePiece (AdminId master v)
         ) => YesodDispatch (Admin master v) master where

    yesodDispatch adm mkey ps mas rLift = case ps of
                  [           ]     -> listR
                  ["page",   x]     -> pageR x
                  ["create"   ]     -> createR
                  ["read",   k]     -> readR   k
                  ["update", k]     -> updateR k
                  ["delete", k]     -> deleteR k
                  _                 -> Nothing
            where run rt act = yesodRunner adm mas rLift mkey (Just rt) act
                  listR      = runWith run  AdminListR getAdminListR
                  pageR   x  = runWith' run AdminPageR getAdminPageR x
                  createR    = runWith  run AdminCreateR getAdminCreateR
                  readR   k  = runWith' run AdminReadR   getAdminReadR   k
                  updateR k  = runWith' run AdminUpdateR getAdminUpdateR k
                  deleteR k  = runWith' run AdminDeleteR getAdminDeleteR k

type Runner sub master =  Route sub 
                       -> (GHandler sub master ChooseRep)
                       -> Application

runWith :: Runner sub master 
        -> Route sub 
        -> GHandler sub master RepHtml
        -> Maybe Application
runWith runner rt act = Just . runner rt $ fmap chooseRep act

runWith' :: SinglePiece a
           =>  Runner sub master                  -- ^ The runner
           -> (a -> Route sub)                    -- ^ The route
           -> (a -> GHandler sub master RepHtml)  -- ^ The action
           -> Text                                -- ^ The piece
           -> Maybe Application                   -- ^ The result
runWith' runner rtf actf txt = do arg <- fromSinglePiece txt
                                  runWith runner (rtf arg) (actf arg)
