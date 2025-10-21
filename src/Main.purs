module Main
  ( main
  , renderBook
  )
  where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (className)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Array (concat, drop, head, last, range, take, zip)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Console (log)
import MyPrelude (both, justs)
import Partial.Unsafe (unsafePartial)
import PrettyPrint (pretty)
import Role (Role)
import RoleBook (RoleGrid, roleBook)
import SortOrder (equalAbilitySortOrder, getSortOrder)

main ∷ Effect Unit
main = do
    log "starting..."
    runWidgetInDom "main" 
        $ renderBook


renderBook :: forall a. Widget HTML a
renderBook = do
    let ps = zip (range 1 1000) roleBook
    let ps' = map (uncurry renderPage) ps
    D.div' $ 
        -- [ D.text "HEJ!"
        -- , D.p' [D.text $ "Pages: " <> show (length ps)]
        -- ] 
        [ D.div
            [className "a4border"] 
            (take 8 ps' )
        , D.div
            [className "a4border"] 
            (drop 8 ps')]

    -- traverse (uncurry renderPage) (zip (range 1 1000) roleBook)
    
renderPage :: Int -> RoleGrid -> forall a. Widget HTML a
renderPage page grid = D.div
    [ className "roleGrid"] 
    [ D.div [className "pageInfo"]
        [ D.h2' [D.text $ show page] 
        , D.h3 [className "roleInfo"]
            roleInfo
        ]        
    , D.div [className "roles"]
        (map renderRow grid) 
    ]
    where
        roles = justs $ concat grid
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

renderRow :: Array (Maybe Role) -> forall a. Widget HTML a
renderRow roles = D.div 
    [className "roleRow"] 
    (map renderRole roles)
    
renderRole :: Maybe Role -> forall a. Widget HTML a
renderRole (Just r) = D.div
    [ className "role"]
    [ D.img [ P.src ("img/Icon_" <> r.id <> ".png") ]
    , D.h3' [D.text r.name]
    ]
renderRole Nothing = D.div [className "role"] []