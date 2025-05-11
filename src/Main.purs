module Main
  where

import Prelude 
import MyPrelude (center, joinString, lpad, rep, segmented)

import Data.Array (concat, nub, range, zip)
import Data.Foldable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Role (Role, allroles)
import SortOrder (SortOrder(..), getSortOrder, sorted)


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
  where ppRole r = lpad 18 r.name 
                <> lpad 12 (show r.team) 
                <> show (getSortOrder r)

printBook :: Effect Unit
printBook = do
  log "All pages!"
  let rowPages :: Array (Array (Array Role))
      rowPages = map (segmented 3)
        $ segmented 12 (sorted allroles)
  
  sequence_ $ map printPage (zip (range 1 1000) rowPages)

  where 
    showRow :: Array Role -> String
    showRow = joinString <<< map (\r -> lpad 18 r.name) 
    printPage :: Tuple Int (Array (Array Role)) -> Effect Unit
    printPage (Tuple i page) = do
      let roles = concat page
          saoAT = (\(SortOrder _ at _ _) -> at) <<< getSortOrder
          sorts = nub $ map (\r -> Tuple r.team (saoAT r)) roles
      
      -- Header
      log (joinString 
        [ rep 22 '-'
        , center 10 ("Page " <> show i)
        , rep 22 '-' 
        ]) 
      
      -- Matching sort keys
      log "Sort Matches per team: "
      sequence_ $ flip map sorts $ \(Tuple t at) -> do
        log ("\t" <> lpad 12 (show t) <> show at) 

      -- Roles
      sequence_ $ map (log <<< showRow) page
      log ""