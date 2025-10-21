module Role
  ( Role(..)
  , CharacterType(..)
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.String as Str
import Foreign (ForeignError(..), fail)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type Role = {
    id :: String,
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
