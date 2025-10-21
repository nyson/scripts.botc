module RoleBook where

import Prelude

import Data.Array (concatMap, groupBy)
import Data.Array.NonEmpty (toArray)
import MyPrelude (segmented)
import OfficialRoles as OfficialRoles
import Role (Role)
import SortOrder (sorted)

type RoleGrid = Array (Array Role)

type RoleBook = Array RoleGrid

roleBook :: RoleBook
roleBook =
  let roleset = sorted OfficialRoles.roles
      teamGrouping :: Role -> Role -> Boolean 
      teamGrouping a b = a.team == b.team 
      rolesByCategory :: Array (Array Role)
      rolesByCategory = map toArray $ groupBy teamGrouping roleset
      rowPages :: Array Role -> Array (Array (Array Role))
      rowPages rs = map (segmented 3)
        $ segmented 12 rs
  in concatMap rowPages rolesByCategory
