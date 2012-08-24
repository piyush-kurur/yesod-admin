{-

A module to generate error messages.

-}

module Yesod.Admin.TH.Error
       ( errMsg
       , combineErrs
       ) where
       
import Data.List

{- Error message generation -}

errMsg :: [String] -> String
errMsg = intercalate ":"

combineErrs :: String -> [String] -> [String]
combineErrs _   []     = []
combineErrs tag (x:xs) = [unlines $ f:map (indent l) xs]
         where f = tag ++ ": " ++ x
               l = length tag + 2
               indent len = (++) $ replicate len ' '
               

{- End of error message generation -}

             