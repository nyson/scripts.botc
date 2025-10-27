module Role
  ( CharacterType(..)
  , Role(..)
  , RoleId
  , value
  )
  where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe)
import Data.String as Str
import Foreign (ForeignError(..), fail)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


type Role = {
    id :: RoleId,
    name :: String,
    edition :: String,
    team :: CharacterType,
    firstNightReminder :: String,
    otherNightReminder :: String,
    reminders :: Array String,
    remindersGlobal :: Maybe(Array String),
    setup :: Boolean,
    ability :: String
}

newtype RoleId = RoleId String

derive instance eqRoleId :: Eq RoleId
instance Hashable RoleId where
  hash (RoleId id) = hash id

value :: RoleId -> String
value (RoleId s) = s

instance ReadForeign RoleId where
  readImpl json = do
    str :: String <- readImpl json
    pure $ RoleId str

data CharacterType = Townsfolk | Outsider | Minion | Demon | Traveller

derive instance eqCharacterType :: Eq CharacterType
derive instance ordCharacterType :: Ord CharacterType
instance showCharacterType :: Show CharacterType where
  show Townsfolk = "Townsfolk"
  show Outsider = "Outsider"
  show Minion = "Minion"
  show Demon = "Demon"
  show Traveller = "Traveler"

instance ReadForeign CharacterType where
  readImpl json = do
    str :: String <- readImpl json
    case Str.toLower str of
      "townsfolk" -> pure Townsfolk
      "outsider" -> pure Outsider
      "minion" -> pure Minion
      "demon" -> pure Demon
      "traveller" -> pure Traveller
      _ -> fail $ ForeignError $ "No parse (CharacterType): " <> str

instance WriteForeign CharacterType where
  writeImpl = writeImpl <<< case _ of
    Townsfolk -> "townsfolk"
    Outsider -> "outsider"
    Minion -> "minion"
    Demon -> "demon"
    Traveller -> "traveller"
