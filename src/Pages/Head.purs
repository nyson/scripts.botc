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
import Data.Array (concat, filter, concatMap)
import Pages.Pamflets as Pamflets
import Pages.RoleBook as RoleBookPage
import RoleBook as Roles

data PageState = Head | RoleBook | Pamflets

startingPage :: PageState
startingPage = Pamflets

page :: forall a. PageState -> Widget HTML a
page p = do
    newState <- D.div'  
            [ D.div'
                [ D.button [P.onClick] [D.text "Head"] $> Head
                , D.button [P.onClick] [D.text "RoleBook"] $> RoleBook
                , D.button [P.onClick] [D.text "Pamflets"] $> Pamflets
                ]
            , D.div' 
                [ case p of
                    RoleBook -> RoleBookPage.renderBook Roles.roleBook []
                    Pamflets -> Pamflets.page 
                        (filter (\r -> r.edition == "bmr") 
                            $ concatMap concat Roles.roleBook)
                    Head -> D.h2' [D.text "Welcome!"]
                ]
            ]
    page newState
