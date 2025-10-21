module Pages.Head
  ( PageState
  , page
  , startingPage
  )
  where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Pages.RoleBook as RoleBookPage
import RoleBook as Roles

data PageState = Head | RoleBook

startingPage :: PageState
startingPage = Head

page :: forall a. PageState -> Widget HTML a
page p = do
    newState <- case p of
        RoleBook -> RoleBookPage.renderBook Roles.roleBook []
        Head -> D.div'  
            [ D.h2' [D.text "Welcome!"]
            , D.button [P.onClick] [D.text "Load index"] $> RoleBook
            ]
    page newState
