module SortOrder
  ( AbilityLength(..)
  , AbilityText(..)
  , AbilityType
  , Alignment(..)
  , Condition(..)
  , NameLength(..)
  , Object(..)
  , Phase(..)
  , Verb(..)
  , SortOrder(..)
  , equalAbilitySortOrder
  , getSortOrder
  , sorted
  )
  where

import Prelude

import Data.Array.Extra (sortOn)
import Data.Either (Either(..))
import Data.String as Str
import Parsing (Parser, runParser)
import Parsing.Combinators (choice)
import Parsing.String (string)
import PrettyPrint (class PrettyPrint)
import Role (CharacterType(..), Role)

data Alignment = Good | Evil

derive instance eqAlignment :: Eq Alignment
derive instance ordAlignment :: Ord Alignment
instance showAlignment :: Show Alignment where
  show Good = "Good"
  show Evil = "Evil"


data Phase
    = AtDuskStar
    | AtNight
    | AtNightStar
    | Day
    | FirstNight
    | FirstDay
    | FirstTime

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

data SortOrder = SortOrder 
  CharacterType
  AbilityType 
  AbilityLength 
  NameLength
derive instance eqSortOrder :: Eq SortOrder
derive instance ordSortOrder :: Ord SortOrder

instance showSortOrder :: Show SortOrder where
  show (SortOrder team ability abilityLength nameLength) = "SortOrder " 
    <> show team 
    <> " [" <> show ability 
    <> "] (" <> show abilityLength 
    <> "," <> show nameLength
    <> ")"

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

equalAbilitySortOrder 
  :: forall r. {name :: String, ability :: String, team :: CharacterType | r} 
    -> {name :: String, ability :: String, team :: CharacterType | r} 
    -> Boolean
equalAbilitySortOrder l r 
  = let (SortOrder _ lAb _ _) = getSortOrder l
        (SortOrder _ rAb _ _) = getSortOrder r
    in lAb == rAb
     

getSortOrder :: forall r. {name :: String, ability :: String, team :: CharacterType | r} -> SortOrder
getSortOrder {name, ability, team}
  = SortOrder
      team
      (parsedAbilityType ability)
      (AbilityLength $ Str.length ability)
      (NameLength $ Str.length name)
  where
    parsedAbilityType :: String -> AbilityType
    parsedAbilityType s = case runParser s parseAbilityType of
      Left _ -> Uncategorized
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
  where prefixP = string "Once per game, " *> choice (map string ["at ", "during "]) 

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
    , string "Traveler" *> pure Traveller
    , string "traveler" *> pure Traveller
    , string "Traveller" *> pure Traveller
    , string "traveller" *> pure Traveller
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
    ]

newtype AbilityText = AbilityText String

sorted :: Array Role -> Array Role
sorted = sortOn getSortOrder

instance PrettyPrint SortOrder where
  pretty (SortOrder _ ab _ _) = case ab of
    StartKnowing -> "You start knowing"
    Each (AtNight) -> "Each night"
    Each (Day) -> "Each day"
    OncePerGame (Day) -> "Once per game, at day"
    OncePerGame AtNightStar -> "Once per game, at night*"
    If TheDemonKills -> "If the Demon kills"
    If ConditionYouAny -> "If you"
    If ConditionAny -> "If"
    When YouLearnThatYouDied -> "When you learn that you died"
    OnYour FirstNight -> "On your first night"
    Uncategorized -> "Uncategorized"
    Each AtNightStar -> "Each night*"
    _ -> ""