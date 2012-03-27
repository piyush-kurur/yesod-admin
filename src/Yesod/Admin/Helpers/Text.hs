{-# LANGUAGE OverloadedStrings  #-}
{-|

Some helper function to deal with text.

-}
module Yesod.Admin.Helpers.Text
       ( capitalise
       , unCapitalise
       , camelCase
       , camelCaseUnwords
       , unCamelCase
       , unCamelCaseWords
       ) where
import Data.Char
import qualified Data.Text as T

type Text = T.Text

-- | Like unwords but combines words according to camel case
-- convention.

camelCaseUnwords :: [Text] -> Text
camelCaseUnwords (x:xs) = T.concat $ x : map capitalise xs
camelCaseUnwords [] = ""

-- | Convert a space seperated string to camel case notation.
camelCase :: Text -> Text
camelCase = camelCaseUnwords . T.words 

-- | Like words but uses camel case convention to determine word
-- boundaries.
unCamelCaseWords :: Text -> [Text]
unCamelCaseWords = lower . T.groupBy breaker 
       where breaker _  = not . isUpper 
             lower (x:xs) = x : map T.toLower xs
             lower []     = []

-- | From camel case to normal convention.
unCamelCase :: Text -> Text
unCamelCase = T.unwords . unCamelCaseWords

-- | Capitalise the first letter
capitalise :: Text -> Text
capitalise = maybe "" upcase . T.uncons
    where upcase (x,t) = toUpper x `T.cons` t

unCapitalise :: Text -> Text
unCapitalise = maybe "" downcase . T.uncons
    where downcase (x,t) = toLower x `T.cons` t