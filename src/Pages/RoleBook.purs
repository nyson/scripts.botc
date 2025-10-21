module Pages.RoleBook
  ( renderBook
  )
  where

import Prelude

import Concur.React.DOM as D
import Concur.React.Props as P

import Concur.Core (Widget)
import Concur.React (HTML)
import Data.Array (concat, drop, head, last, range, take, zip)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), uncurry)
import MyPrelude (both, fillWith)
import Partial.Unsafe (unsafePartial)
import PrettyPrint (pretty)
import Role (Role)
import RoleBook (RoleBook, RoleGrid)
import SortOrder (equalAbilitySortOrder, getSortOrder)

renderBook :: forall a. RoleBook -> Array Role -> Widget HTML a
renderBook rb _ = do
    let ps = zip (range 1 1000) rb
    let ps' = map (uncurry renderPage) ps
    D.div' $
        [ D.div
            [P.className "a4border"]
            (take 8 ps' )
        , D.div
            [P.className "a4border"]
            (drop 8 ps')]

renderPage :: Int -> RoleGrid -> forall a. Widget HTML a
renderPage page grid = D.div
    [ P.className "roleGrid"]
    [ D.div [P.className "pageInfo"]
        [ D.h2' [D.text $ show page]
        , D.h3 [P.className "roleInfo"]
            roleInfo
        ]
    , D.div [P.className "roles"]
        (map renderRow grid)
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

renderRow :: Array Role -> forall a. Widget HTML a
renderRow roles = D.div
    [P.className "roleRow"]
    (map renderRole filledRoles)
    where filledRoles = fillWith 3 (map Just roles) Nothing

renderRole :: Maybe Role -> forall a. Widget HTML a
renderRole Nothing = D.div [P.className "role"] []
renderRole (Just r) = D.div
    [ P.className "role" ]
    [ D.img [ P.src ("assets/img/Icon_" <> r.id <> ".png") ]
    , D.h3' [D.text r.name]
    ]
