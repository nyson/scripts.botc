module MainCLI
  ( bookPage
  , main
  , printList
  , printPage
  )
  where

import Prelude

import Data.Array (concat, index, nub, range, zip)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import MyPrelude (center, joinString, justs, lpad, rep, segmented)
import OfficialRoles as OfficialRoles
import Render (render)
import Role (Role)
import RoleBook (RoleBook, RoleGrid)
import SortOrder (SortOrder(..), getSortOrder, sorted)

main :: Effect Unit
main = do
  log "All roles!"
  let segRoles = segmented 12 (sorted OfficialRoles.roles)
  sequence_ $ map printList (zip (range 1 1000) segRoles)
  render

printList :: Tuple Int (Array Role) -> Effect Unit
printList (Tuple i rs) = do 
  log $ "Page " <> show i
  sequence_ $ map (log <<< ppRole) rs
  log ""
  where ppRole r
          = lpad 18 r.name 
            <> lpad 12 (show r.team) 
            <> show (getSortOrder r)
                
bookPage :: Int -> RoleBook -> Effect Unit
bookPage p book = case index book p of
  Nothing -> log "No pages here!"
  Just page -> printPage p page

printPage :: Int -> RoleGrid -> Effect Unit
printPage i page = do
  let roles = concat page
      saoAT = (\(SortOrder _ at _ _) -> at) <<< getSortOrder
      sorts = nub $ map (\r -> Tuple r.team (saoAT r)) (justs roles)

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

  where 
    showRow :: Array (Maybe Role) -> String
    showRow = joinString <<< map (\r -> lpad 18 r.name) <<< justs