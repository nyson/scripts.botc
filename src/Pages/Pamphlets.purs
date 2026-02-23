module Pages.Pamphlets
  ( page
  )
  where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Array.Extra (sortOn)
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Pages.State (RoleFilter)
import Role (Role, value)
import SortOrder (getSortOrder)

page :: forall a. RoleFilter -> Widget HTML a 
page Nothing = D.h2' [D.text "No roles selected!"]
page (Just roles) = D.div 
    [P.className "rolePamphlets"]
    (map renderRole (sortOn getSortOrder $ HM.values roles))


renderRole :: forall a. Role -> Widget HTML a
renderRole role = D.div
    [ P.className "rolePamphlet" ]
    [ D.img [P.src ("assets/img/Icon_" <> value role.id <> ".png")]
    , D.h3' [D.text role.name ]
    , D.p' [D.text role.edition]
    , D.p' [D.text $ show role.team]
    , D.p' [D.text role.ability ]
    -- , D.p' [D.text $ 
    --     if role.setup 
    --     then "Affects setup"
    --     else "Doesn't affect setup"]
    -- , D.p' [D.text role.firstNightReminder ]
    -- , D.p' [D.text role.otherNightReminder ]
    ]