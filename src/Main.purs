module Main
  where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.List (List(..), range, filter, foldr)
import Data.Foldable (sum)

main :: Effect Unit
main = log "🍝"

divides :: Int -> Int -> Boolean
divides x y = y `mod` x == 0

or' :: forall a. Array (a -> Boolean) -> a -> Boolean
or' fs x = foldr (\f acc -> f x || acc) false fs

ns :: List Int
ns = range 0 999 

multiplies :: List Int -> List Int
multiplies = filter (or' [divides 3, divides 5])

answer :: Int
answer = sum (multiplies ns)