module SortOrder
  ( SortOrder(..)
  , AbilityType(..)
  , AbilityLength(..)
  , NameLength(..)
  , AbilityText(..)
  , Alignment(..)
  , CharacterType(..)
  , Condition(..)
  , Object(..)
  , Phase(..)
  , Verb(..)
  , getSortOrder
  )
  where

import Prelude

import Parsing(Parser(..), runParser)
import Parsing.Combinators (choice)
import Parsing.String (string, char, anyChar)
import Data.Either (Either(..), fromRight)
import Data.Tuple (Tuple(..))
import Data.Array (many, length)
import Data.String as Str

data Alignment = Good | Evil

derive instance eqAlignment :: Eq Alignment
derive instance ordAlignment :: Ord Alignment
instance showAlignment :: Show Alignment where
  show Good = "Good"
  show Evil = "Evil"

data CharacterType = Townsfolk | Outsider | Minion | Demon | Traveler | UnknownTeam

derive instance eqCharacterType :: Eq CharacterType
derive instance ordCharacterType :: Ord CharacterType
instance showCharacterType :: Show CharacterType where
  show Townsfolk = "Townsfolk"
  show Outsider = "Outsider"
  show Minion = "Minion"
  show Demon = "Demon"
  show Traveler = "Traveler"
  show UnknownTeam = "UnknownTeam"


data Phase
    = AtDuskStar
    | AtNight
    | AtNightStar
    | Day
    | FirstNight
    | FirstDay
    | FirstTime
    | AnyPhase

derive instance eqPhase :: Eq Phase
derive instance ordPhase :: Ord Phase
instance showPhase :: Show Phase where
  show AtDuskStar = "AtDuskStar"
  show AtNight = "AtNight"
  show AtNightStar = "AtNightStar"
  show Day = "Day"
  show FirstNight = "FirstNight"
  show FirstDay = "FirstDay"
  show FirstTime = "FirstTime"
  show AnyPhase = "AnyPhase"

data Verb
    = Think
    | Are
    | Have
    | DoNotKnow
    | Might
    | YouAny

derive instance eqVerb :: Eq Verb
derive instance ordVerb :: Ord Verb
instance showVerb :: Show Verb where
  show Think = "Think"
  show Are = "Are"
  show Have = "Have"
  show DoNotKnow = "DoNotKnow"
  show Might = "Might"
  show YouAny = "YouAny"

data Condition
    = YouDie
    | YouDied
    | YouLearnThatYouDied
    | YouAreMad
    | ConditionYouAny
    | TheDemonDies
    | TheDemonKills
    | TheDemonAny
    | Both
    | FiveOrMorePlayersAlive
    | ConditionAny

derive instance eqCondition :: Eq Condition
derive instance ordCondition :: Ord Condition

instance showCondition :: Show Condition where
  show YouDie = "YouDie"
  show YouDied = "YouDied"
  show YouLearnThatYouDied = "YouLearnThatYouDied"
  show YouAreMad = "YouAreMad"
  show ConditionYouAny = "ConditionYouAny"
  show TheDemonDies = "TheDemonDies"
  show TheDemonKills = "TheDemonKills"
  show TheDemonAny = "TheDemonAny"
  show Both = "Both"
  show FiveOrMorePlayersAlive = "FiveOrMorePlayersAlive"
  show ConditionAny = "ConditionAny"

data Object
    = Alignment Alignment
    | Players
    | CharacterType CharacterType
    | AnyObject

derive instance eqObject :: Eq Object
derive instance ordObject :: Ord Object
instance showObject :: Show Object where
  show (Alignment a) = "Alignment " <> show a
  show Players = "Players"
  show (CharacterType c) = "CharacterType " <> show c
  show AnyObject = "AnyObject"

newtype NameLength = NameLength Int

derive instance eqNameLength :: Eq NameLength
derive instance ordNameLength :: Ord NameLength
instance showNameLength :: Show NameLength where
  show (NameLength l) = "NameLength " <> show l

newtype AbilityLength = AbilityLength Int

derive instance eqAbilityLength :: Eq AbilityLength
derive instance ordAbilityLength :: Ord AbilityLength
instance showAbilityLength :: Show AbilityLength where
  show (AbilityLength l) = "AbilityLength " <> show l

data SortOrder = SortOrder CharacterType AbilityType AbilityLength NameLength

derive instance eqSortOrder :: Eq SortOrder
derive instance ordSortOrder :: Ord SortOrder

instance showSortOrder :: Show SortOrder where
  show (SortOrder team abilityType (AbilityLength alen) (NameLength nlen)) 
    = "SortOrder " <> show team <> " [" <> show abilityType <> "] (" <> show alen <> "," <> show nlen <> ")"

data AbilityType
    = StartKnowing
    | SOAtNight -- ?
    | Each Phase  
    | OncePerGame Phase
    | OnYour Phase
    | You Verb
    | When Condition
    | If Condition 
    | All Object
    | The Phase
    | References Object
    | Uncategorized

derive instance eqAbilityType :: Eq AbilityType
derive instance ordAbilityType :: Ord AbilityType
instance showAbilityType :: Show AbilityType where
  show StartKnowing = "StartKnowing"
  show SOAtNight = "SOAtNight"
  show (Each c) = "Each " <> show c
  show (OncePerGame c) = "OncePerGame " <> show c
  show (OnYour c) = "OnYour " <> show c
  show (You v) = "You " <> show v
  show (When c) = "When " <> show c
  show (If c) = "If " <> show c
  show (All o) = "All " <> show o
  show (The c) = "The " <> show c
  show (References o) = "References " <> show o
  show (Uncategorized) = "Uncategorized"

getSortOrder :: forall r. {name :: String, ability :: String, team :: String | r} -> SortOrder
getSortOrder {name, ability, team}
  = SortOrder
    (parsedTeam team)
    (parsedAbilityType ability)
    (AbilityLength $ Str.length ability)
    (NameLength $ Str.length name)
  where
    parsedTeam :: String -> CharacterType
    parsedTeam s = fromRight UnknownTeam (flip runParser parseCharacterType s)

    parsedAbilityType :: String -> AbilityType
    parsedAbilityType s = case runParser s parseAbilityType of
      Left err -> Uncategorized
      Right result -> result

getAbilityType :: String -> Tuple AbilityType Int
getAbilityType str = Tuple (parsed str) (Str.length str)
  where 
    parsed :: String -> AbilityType
    parsed s = case runParser s parseAbilityType of
      Left err -> Uncategorized
      Right result -> result


parseAbilityType :: Parser String AbilityType
parseAbilityType = choice 
    [ parseStartKnowing
    , parseEach
    , parseOncePerGame
    , parseOnYour
    , parseYou
    , parseWhen
    , parseIf
    , parseAll
    , parseThe
    , parseSOAtNight
    , parseReferences
    , pure Uncategorized
    ]

parseStartKnowing :: Parser String AbilityType
parseStartKnowing = string "You start knowing" *> pure StartKnowing

parseEach :: Parser String AbilityType
parseEach = string "Each " *> (Each <$> parsePhase)

parseOncePerGame :: Parser String AbilityType
parseOncePerGame = prefixP *> (OncePerGame <$> parsePhase) 
    where prefixP = choice 
                    [ string "Once per game, at "
                    , string "Once per game, during "
                    ]

parseOnYour :: Parser String AbilityType
parseOnYour = string "On your " *> (OnYour <$> parsePhase)

parseYou :: Parser String AbilityType
parseYou = string "You " *> (You <$> parseVerb)

parseWhen :: Parser String AbilityType
parseWhen = string "When " *> (When <$> parseCondition)

parseIf :: Parser String AbilityType
parseIf = string "If " *> (If <$> parseCondition)

parseAll :: Parser String AbilityType
parseAll = string "All " *> (All <$> parseObject)

parseThe :: Parser String AbilityType
parseThe = string "The " *> (The <$> parsePhase)

parseReferences :: Parser String AbilityType
parseReferences = References <$> parseObject

parseSOAtNight :: Parser String AbilityType
parseSOAtNight = SOAtNight <$ string "At night"

parseObject :: Parser String Object
parseObject = choice
    [ Alignment <$> parseAlignment
    , Players <$ string "players"
    , Players <$ string "Players"
    , CharacterType <$> parseCharacterType
    ]

parseCharacterType :: Parser String CharacterType
parseCharacterType = choice
    [ string "Townsfolk" *> pure Townsfolk
    , string "townsfolk" *> pure Townsfolk
    , string "Outsider" *> pure Outsider
    , string "outsider" *> pure Outsider
    , string "minion" *> pure Minion
    , string "Minion" *> pure Minion
    , string "Demon" *> pure Demon
    , string "demon" *> pure Demon
    , string "Traveler" *> pure Traveler
    , string "traveler" *> pure Traveler
    , string "Traveller" *> pure Traveler
    , string "traveller" *> pure Traveler
    ]

parseAlignment :: Parser String Alignment
parseAlignment = choice
    [ string "good" *> pure Good
    , string "Good" *> pure Good
    , string "evil" *> pure Evil
    , string "Evil" *> pure Evil
    ]

parseCondition :: Parser String Condition
parseCondition = choice
    [ string "you die" *> pure YouDie
    , string "you died" *> pure YouDied
    , string "you learn that you died" *> pure YouLearnThatYouDied
    , string "you are \"mad\"" *> pure YouAreMad
    , string "you" *> pure ConditionYouAny
    , string "the Demon dies" *> pure TheDemonDies
    , string "the Demon kills" *> pure TheDemonKills
    , string "the Demon" *> pure TheDemonAny
    , string "both" *> pure Both
    , string "there are 5 or more players alive" *> pure FiveOrMorePlayersAlive
    , pure ConditionAny
    ]

parseVerb :: Parser String Verb
parseVerb = choice
    [ Think <$ string "think"
    , Are <$ string "are"
    , Have <$ string "have"
    , DoNotKnow <$ string "do not know"
    , Might <$ string "might"
    , pure YouAny
    ]

parsePhase :: Parser String Phase
parsePhase = choice
    [ AtDuskStar <$ string "dusk*"
    , AtNightStar <$ string "night*"
    , AtNight <$ string "night"
    , Day <$ string "day"
    , Day <$ string "the day"
    , FirstNight <$ string "1st night"
    , FirstDay <$ string "1st day"
    , FirstTime <$ string "1st time"
    , pure AnyPhase
    ]

newtype AbilityText = AbilityText String

