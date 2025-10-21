module Pages.Pamflets
  ( page
  )
  where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Role (Role)

page :: forall a. Array Role -> Widget HTML a 
page roles = D.div' 
    [ D.h2' [D.text "Hello there!"]
    , D.div' (map renderRole roles)
    ]

renderRole :: forall a. Role -> Widget HTML a
renderRole role = D.div' 
    [ D.h3' [D.text role.name ]
    , D.img [P.src ("assets/img/Icon_" <> role.id <> ".png")]
    , D.p' [D.text role.edition]
    , D.p' [D.text $ show role.team]
    , D.p' [D.text role.ability ]
    , D.p' [D.text $ 
        if role.setup 
        then "Affects setup"
        else "Doesn't affect setup"]
    , D.p' [D.text role.firstNightReminder ]
    , D.p' [D.text role.otherNightReminder ]
    ]