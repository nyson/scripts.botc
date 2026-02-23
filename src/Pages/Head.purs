module Pages.Head
  ( header
  , page
  , startingPage
  )
  where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Array (concat, filter)
import Data.HashMap as HM
import Data.Maybe (Maybe(..), maybe)
import OfficialRoles as OfficialRoles
import Pages.Pamphlets as Pamphlets
import Pages.RoleBook as RoleBookPage
import Pages.State (PageState(..), State, RoleFilter)
import React.DOM.Dynamic (script)
import Role (Role)
import RoleBook as RoleBook
import Web.DOM.Document (doctype)

byEdition :: Maybe String -> RoleFilter
byEdition Nothing = Nothing
byEdition (Just ed) = Just $ HM.fromArrayBy 
    (\r -> r.id)
    (\r -> r) 
    $ filter 
        (\r -> r.edition == ed) 
        OfficialRoles.roles

startingPage :: State
startingPage = {
    page: RoleBook,
    roleBook: RoleBook.roleBook,
    edition: Just "bmr",
    selectedRoles: byEdition (Just "bmr")
}


page :: forall a. State -> Widget HTML a
page p = do
    newState <- D.div'  
        [ header p
        , D.div' 
            [ case p.page of
                RoleBook -> RoleBookPage.renderBook p.roleBook p.selectedRoles
                Pamphlets -> Pamphlets.page p.selectedRoles
                Head -> D.h2' [D.text "Welcome!"]
            ]
        ]
    page newState


selectScript :: State -> Widget HTML State
selectScript p = do
    newVal <- D.select 
        [ P.onChange ]  
        opts
    pure $ p {edition = newVal }
    where 
        opts :: forall a. Array (Widget HTML a)
        opts = map scriptOption [ Just "tb", Just "bmr", Just "snv", Nothing ]
        toL = maybe "nothing" (\x -> x)
        fromL ed = case ed of 
            "tb" -> Just "tb"
            "bmr" -> Just "bmr"
            "snv" -> Just "snv"
            _ -> Nothing
        scriptOption s = do
            let nameOf n = case n of
                    "tb" -> "Trouble Brewing"
                    "bmr" -> "Bad Moon Rising"
                    "snv" -> "Sects and Violets"
                    e -> "invalid edition: " <> e 
                scriptName :: String
                scriptName = maybe "Nothing" nameOf s 
            D.option'
                [ D.text scriptName]

header :: State -> Widget HTML State
header p = D.ul
    [ P.className "noprint" ]
    $ concat [
        [ selectScript p ], 
        map option [ Head, RoleBook, Pamphlets ]
    ]
    where 
        option :: PageState -> Widget HTML State
        option val = 
            D.li 
            [ P.onClick
            , P.className
                if p.page == val 
                then "option selected"
                else "option"
            ] 
            [ D.text $ nameOf val ] 
            $> p { page = val }
        nameOf Head = "Head"
        nameOf RoleBook = "Role Book"
        nameOf Pamphlets = "Pampflets"
        
            