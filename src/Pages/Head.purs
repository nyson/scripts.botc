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
import Pages.Pamphlets as Pamphlets
import Pages.RoleBook as RoleBookPage
import RoleBook as Roles

data PageState = Head | RoleBook | Pamphlets

derive instance eqPageState :: Eq PageState


startingPage :: PageState
startingPage = Pamphlets

page :: forall a. PageState -> Widget HTML a
page p = do
    newState <- D.div'  
        [ header p
        , D.div' 
            [ case p of
                RoleBook -> RoleBookPage.renderBook Roles.roleBook []
                Pamphlets -> Pamphlets.page 
                    (filter (\r -> r.edition == "bmr") 
                        $ concatMap concat Roles.roleBook)
                Head -> D.h2' [D.text "Welcome!"]
            ]
        ]
    page newState

header :: PageState -> Widget HTML PageState
header p = D.ul'
    [ option p Head "Head"
    , option p RoleBook "Role Book"
    , option p Pamphlets "Pamphlets"
    ]
    where option st val name = 
            D.li 
            [ P.onClick
            , P.className
                if st == val 
                then "option selected"
                else "option"
            ] 
            [D.text name] 
            $> val
        
            