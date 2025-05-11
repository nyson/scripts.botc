module MyPrelude where

import Prelude

import Data.Array (drop, head, replicate, take, (:))
import Data.Maybe (fromMaybe)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray)

rep :: Int -> Char -> String
rep i c = fromCharArray $ replicate i c

repSpace :: Int -> String
repSpace = flip rep ' '

lpad :: Int -> String -> String
lpad i s = s <> repSpace (i - Str.length s)

center :: Int -> String -> String
center i s = repSpace (halfFill + overflow) 
    <> s 
    <> repSpace halfFill
  where 
    halfFill = max ((i - Str.length s) `div` 2) 0
    overflow = (i - Str.length s) `mod` 2

segmented :: forall a. Int -> Array a -> Array (Array a)
segmented _ [] = []
segmented i xs = take i xs: segmented i (drop i xs)

joinStringWith :: String -> Array String -> String
joinStringWith _   [] = ""
joinStringWith sep ss 
    = fromMaybe "" (head ss) <> case drop 1 ss of
        [] -> ""
        ss' -> sep <> (joinStringWith sep ss')

joinString :: Array String -> String
joinString = joinStringWith ""
