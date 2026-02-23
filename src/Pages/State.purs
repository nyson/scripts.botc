module Pages.State where

import Prelude

import Data.HashMap (HashMap)
import Data.Maybe (Maybe)
import Role (RoleId, Role)
import RoleBook (RoleBook)

data PageState = Head | RoleBook | Pamphlets
derive instance eqPageState :: Eq PageState

type RoleFilter = Maybe (HashMap RoleId Role)


type State = {
    page :: PageState,
    roleBook :: RoleBook,
    edition :: Maybe String,
    selectedRoles :: RoleFilter
}
