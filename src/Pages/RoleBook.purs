module Pages.RoleBook
  ( renderBook
  )
  where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Array (concat, drop, head, last, range, take, zip)
import Data.HashMap (member)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), uncurry)
import MyPrelude (both, fillWith)
import Pages.State (RoleFilter)
import Partial.Unsafe (unsafePartial)
import PrettyPrint (pretty)
import Role (Role, value)
import RoleBook (RoleBook, RoleGrid)
import SortOrder (equalAbilitySortOrder, getSortOrder)

renderBook :: forall a. RoleBook -> RoleFilter -> Widget HTML a
renderBook rb m = do
    let ps = zip (range 1 1000) rb
    let ps' = map (uncurry (renderPage m)) ps
    D.div' $
        [ D.div
            [P.className "a4border"]
            (take 8 ps' )
        , D.div
            [P.className "a4border"]
            (drop 8 ps')]

renderPage :: RoleFilter -> Int -> RoleGrid -> forall a. Widget HTML a
renderPage m page grid = D.div
    [ P.className "roleGrid"]
    [ D.div [P.className "pageInfo"]
        [ D.h2' [D.text $ show page]
        , D.h3 [P.className "roleInfo"]
            roleInfo
        ]
    , D.div [P.className "roles"]
        (map (renderRow m) grid)
    ]
    where
        roles = concat grid
        (Tuple f l) = both (unsafePartial fromJust)
            $ Tuple (head roles) (last roles)
        saoAbilityText :: Role -> String
        saoAbilityText r = pretty $ getSortOrder r
        roleInfo | equalAbilitySortOrder f l
            = [ D.text $ saoAbilityText f
              , D.text "..."]
        roleInfo
            = [ D.text $ saoAbilityText f
              , D.br'
              , D.text $ "... " <> saoAbilityText l
              ]

renderRow :: RoleFilter -> Array Role -> forall a. Widget HTML a
renderRow m roles = D.div
    [P.className "roleRow"]
    (map (renderRole m) filledRoles)
    where filledRoles = fillWith 3 (map Just roles) Nothing

renderRole :: RoleFilter -> Maybe Role -> forall a. Widget HTML a
renderRole _ Nothing = D.div [P.className "role"] []
renderRole rf (Just r) = D.div
    [ P.className roleClass ]
    [ D.img [ P.src ("assets/img/Icon_" <> value r.id <> ".png") ]
    , D.h3'
        [D.text r.name]
    ]
    where roleClass = case rf of 
            Just (m) | not $ r.id `member` m 
                -> "role deselected"
            _ -> "role"
