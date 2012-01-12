module Yesod.Admin.Helpers
       ( unCamelCase
       , capitalise
       , unCapitalise
       , camelCase
       ) where


import Data.Char
import Data.List
-- Helper functions

unCamelCase :: String -> String
unCamelCase = unwords . map (map toLower) . groupBy breaker
       where breaker _ y = not $ isUpper y

capitalise :: String -> String
capitalise []     = []
capitalise (x:xs) = toUpper x : xs

unCapitalise :: String -> String
unCapitalise []     = []
unCapitalise (x:xs) = toLower x : xs

camelCase :: String -> String
camelCase str = case wds of
                     (x:xs) -> concat (x: map capitalise xs)
                     _      -> str
     where wds = words str
