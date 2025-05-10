module Main
  ( main
  , segmented
  )
  where

import Prelude

import Data.Array (drop, range, replicate, take, zip, (:))
import Data.Foldable (sequence_)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Role (Role, allroles)
import SortOrder (getSortOrder, sorted)

repSpace :: Int -> String
repSpace = fromCharArray <<< flip replicate ' ' 

lpad :: String -> Int -> String
lpad s i = s <> repSpace (i - Str.length s)

center :: String -> Int -> String
center s i = repSpace (halfFill + overflow) <> s <> repSpace halfFill
  where 
    halfFill = max ((i - Str.length s) `div` 2) 0
    overflow = (i - Str.length s) `mod` 2

segmented :: forall a. Int -> Array a -> Array (Array a)
segmented _ [] = []
segmented i xs = take i xs: segmented i (drop i xs)

main :: Effect Unit
main = do
  log "All roles!"
  let segRoles = segmented 12 (sorted allroles)
  sequence_ $ map printList (zip (range 1 1000) segRoles)

printList :: Tuple Int (Array Role) -> Effect Unit
printList (Tuple i rs) = do 
  log $ "Page " <> show i
  sequence_ $ map (log <<< ppRole) rs
  log ""
  where ppRole r = lpad r.name 18 
                <> lpad (show r.team) 12 
                <> show (getSortOrder r)
                <> "\n\t" <> r.ability
                <> "\n"
