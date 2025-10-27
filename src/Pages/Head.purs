module Pages.Head
  ( page
  , startingPage
  )
  where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Array (filter)
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import OfficialRoles as OfficialRoles
import Pages.Pamphlets as Pamphlets
import Pages.RoleBook as RoleBookPage
import Pages.State (PageState(..), State)
import RoleBook as RoleBook


startingPage :: State
startingPage = {
    page: Pamphlets,
    roleBook: RoleBook.roleBook,
    selectedRoles: Just $ HM.fromArrayBy 
        (\r -> r.id)
        (\r -> r) 
        $ filter 
            (\r -> r.edition == "bmr") 
            OfficialRoles.roles
}


page :: forall a. State -> Widget HTML a
page p = do
    newState <- D.div'  
        [ (\newPage -> p {page = newPage}) <$> header p.page
        , D.div' 
            [ case p.page of
                RoleBook -> RoleBookPage.renderBook p.roleBook p.selectedRoles
                Pamphlets -> Pamphlets.page p.selectedRoles
                Head -> D.h2' [D.text "Welcome!"]
            ]
        ]
    page newState

header :: PageState -> Widget HTML PageState
header p = D.ul
    [ P.className "noprint" ]
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
        
            