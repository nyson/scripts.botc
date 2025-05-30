module Role
  ( Role(..)
  , CharacterType(..)
  , allroles
  )
  where

import Prelude

import Data.Either (fromRight)
import Data.Maybe (Maybe)
import Data.String as Str
import Foreign (ForeignError(..), fail)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, readJSON, writeImpl)

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

allroles :: Array Role
allroles = fromRight [] (readJSON allrolesRaw)
  where
    allrolesRaw :: String
    allrolesRaw = """ [
      {
        "id": "washerwoman",
        "name": "Washerwoman",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "Show the character token of a Townsfolk in play. Point to two players, one of which is that character.",
        "otherNightReminder": "",
        "reminders": [
          "Townsfolk",
          "Wrong"
        ],
        "setup": false,
        "ability": "You start knowing that 1 of 2 players is a particular Townsfolk."
      },
      {
        "id": "librarian",
        "name": "Librarian",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "Show the character token of an Outsider in play. Point to two players, one of which is that character.",
        "otherNightReminder": "",
        "reminders": [
          "Outsider",
          "Wrong"
        ],
        "setup": false,
        "ability": "You start knowing that 1 of 2 players is a particular Outsider. (Or that zero are in play.)"
      },
      {
        "id": "investigator",
        "name": "Investigator",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "Show the character token of a Minion in play. Point to two players, one of which is that character.",
        "otherNightReminder": "",
        "reminders": [
          "Minion",
          "Wrong"
        ],
        "setup": false,
        "ability": "You start knowing that 1 of 2 players is a particular Minion."
      },
      {
        "id": "chef",
        "name": "Chef",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "Show the finger signal (0, 1, 2, …) for the number of pairs of neighboring evil players.",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "You start knowing how many pairs of evil players there are."
      },
      {
        "id": "empath",
        "name": "Empath",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "Show the finger signal (0, 1, 2) for the number of evil alive neighbors.",
        "otherNightReminder": "Show the finger signal (0, 1, 2) for the number of evil alive neighbors.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, you learn how many of your 2 alive neighbors are evil."
      },
      {
        "id": "fortuneteller",
        "name": "Fortune Teller",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "The Fortune Teller points to two players. Give the head signal (nod 'yes', shake 'no') for whether one of those players is the Demon.",
        "otherNightReminder": "The Fortune Teller points to two players. Show the head signal (nod 'yes', shake 'no') for whether one of those players is the Demon.",
        "reminders": [
          "Red Herring"
        ],
        "setup": false,
        "ability": "Each night, choose 2 players: you learn if either is a Demon. There is a good player that registers as a Demon to you."
      },
      {
        "id": "undertaker",
        "name": "Undertaker",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If a player was executed today: Show that player's character token.",
        "reminders": [
          "Died Today"
        ],
        "setup": false,
        "ability": "Each night*, you learn which character died by execution today."
      },
      {
        "id": "monk",
        "name": "Monk",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "The previously protected player is no longer protected. The Monk points to a player not themself. Mark that player 'Safe'.",
        "reminders": [
          "Safe"
        ],
        "setup": false,
        "ability": "Each night*, choose a player (not yourself): they are safe from the Demon tonight."
      },
      {
        "id": "ravenkeeper",
        "name": "Ravenkeeper",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If the Ravenkeeper died tonight: The Ravenkeeper points to a player. Show that player's character token.",
        "reminders": [],
        "setup": false,
        "ability": "If you die at night, you are woken to choose a player: you learn their character."
      },
      {
        "id": "virgin",
        "name": "Virgin",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "The 1st time you are nominated, if the nominator is a Townsfolk, they are executed immediately."
      },
      {
        "id": "slayer",
        "name": "Slayer",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, during the day, publicly choose a player: if they are the Demon, they die."
      },
      {
        "id": "soldier",
        "name": "Soldier",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "You are safe from the Demon."
      },
      {
        "id": "mayor",
        "name": "Mayor",
        "edition": "tb",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If only 3 players live & no execution occurs, your team wins. If you die at night, another player might die instead."
      },
      {
        "id": "butler",
        "name": "Butler",
        "edition": "tb",
        "team": "outsider",
        "firstNightReminder": "The Butler points to a player. Mark that player as 'Master'.",
        "otherNightReminder": "The Butler points to a player. Mark that player as 'Master'.",
        "reminders": [
          "Master"
        ],
        "setup": false,
        "ability": "Each night, choose a player (not yourself): tomorrow, you may only vote if they are voting too."
      },
      {
        "id": "drunk",
        "name": "Drunk",
        "edition": "tb",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "remindersGlobal": [
          "Is The Drunk"
        ],
        "setup": true,
        "ability": "You do not know you are the Drunk. You think you are a Townsfolk character, but you are not."
      },
      {
        "id": "recluse",
        "name": "Recluse",
        "edition": "tb",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "You might register as evil & as a Minion or Demon, even if dead."
      },
      {
        "id": "saint",
        "name": "Saint",
        "edition": "tb",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If you die by execution, your team loses."
      },
      {
        "id": "poisoner",
        "name": "Poisoner",
        "edition": "tb",
        "team": "minion",
        "firstNightReminder": "The Poisoner points to a player. That player is poisoned.",
        "otherNightReminder": "The previously poisoned player is no longer poisoned. The Poisoner points to a player. That player is poisoned.",
        "reminders": [
          "Poisoned"
        ],
        "setup": false,
        "ability": "Each night, choose a player: they are poisoned tonight and tomorrow day."
      },
      {
        "id": "spy",
        "name": "Spy",
        "edition": "tb",
        "team": "minion",
        "firstNightReminder": "Show the Grimoire to the Spy for as long as they need.",
        "otherNightReminder": "Show the Grimoire to the Spy for as long as they need.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, you see the Grimoire. You might register as good & as a Townsfolk or Outsider, even if dead."
      },
      {
        "id": "scarletwoman",
        "name": "Scarlet Woman",
        "edition": "tb",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "If the Scarlet Woman became the Demon today: Show the 'You are' card, then the demon token.",
        "reminders": [
          "Is The Demon"
        ],
        "setup": false,
        "ability": "If there are 5 or more players alive & the Demon dies, you become the Demon. (Travellers don't count.)"
      },
      {
        "id": "baron",
        "name": "Baron",
        "edition": "tb",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": true,
        "ability": "There are extra Outsiders in play. [+2 Outsiders]"
      },
      {
        "id": "imp",
        "name": "Imp",
        "edition": "tb",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The Imp points to a player. That player dies. If the Imp chose themselves: Replace the character of 1 alive minion with a spare Imp token. Show the 'You are' card, then the Imp token.",
        "reminders": [
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, choose a player: they die. If you kill yourself this way, a Minion becomes the Imp."
      },
      {
        "id": "thief",
        "name": "Thief",
        "edition": "tb",
        "team": "traveller",
        "firstNightReminder": "The Thief points to a player. Put the Thief's 'Negative Vote' reminder by the chosen player's character token.",
        "otherNightReminder": "The Thief points to a player. Put the Thief's 'Negative Vote' reminder by the chosen player's character token.",
        "reminders": [
          "Negative Vote"
        ],
        "setup": false,
        "ability": "Each night, choose a player (not yourself): their vote counts negatively tomorrow."
      },
      {
        "id": "bureaucrat",
        "name": "Bureaucrat",
        "edition": "tb",
        "team": "traveller",
        "firstNightReminder": "The Bureaucrat points to a player. Put the Bureaucrat's '3 Votes' reminder by the chosen player's character token.",
        "otherNightReminder": "The Bureaucrat points to a player. Put the Bureaucrat's '3 Votes' reminder by the chosen player's character token.",
        "reminders": [
          "3 Votes"
        ],
        "setup": false,
        "ability": "Each night, choose a player (not yourself): their vote counts as 3 votes tomorrow."
      },
      {
        "id": "gunslinger",
        "name": "Gunslinger",
        "edition": "tb",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Each day, after the 1st vote has been tallied, you may choose a player that voted: they die."
      },
      {
        "id": "scapegoat",
        "name": "Scapegoat",
        "edition": "tb",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If a player of your alignment is executed, you might be executed instead."
      },
      {
        "id": "beggar",
        "name": "Beggar",
        "edition": "tb",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "You must use a vote token to vote. If a dead player gives you theirs, you learn their alignment. You are sober & healthy."
      },
      {
        "id": "grandmother",
        "name": "Grandmother",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "Show the marked character token. Point to the marked player.",
        "otherNightReminder": "If the Grandmother's grandchild was killed by the Demon tonight: The Grandmother dies.",
        "reminders": [
          "Grandchild",
          "Dead"
        ],
        "setup": false,
        "ability": "You start knowing a good player & their character. If the Demon kills them, you die too."
      },
      {
        "id": "sailor",
        "name": "Sailor",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "The Sailor points to a living player. Either the Sailor, or the chosen player, is drunk.",
        "otherNightReminder": "The previously drunk player is no longer drunk. The Sailor points to a living player. Either the Sailor, or the chosen player, is drunk.",
        "reminders": [
          "Drunk"
        ],
        "setup": false,
        "ability": "Each night, choose an alive player: either you or they are drunk until dusk. You can't die."
      },
      {
        "id": "chambermaid",
        "name": "Chambermaid",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "The Chambermaid points to two players. Show the number signal (0, 1, 2, …) for how many of those players wake tonight for their ability.",
        "otherNightReminder": "The Chambermaid points to two players. Show the number signal (0, 1, 2, …) for how many of those players wake tonight for their ability.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, choose 2 alive players (not yourself): you learn how many woke tonight due to their ability."
      },
      {
        "id": "exorcist",
        "name": "Exorcist",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "The Exorcist points to a player, different from the previous night. If that player is the Demon: Wake the Demon. Show the Exorcist token. Point to the Exorcist. The Demon does not act tonight.",
        "reminders": [
          "Chosen"
        ],
        "setup": false,
        "ability": "Each night*, choose a player (different to last night): the Demon, if chosen, learns who you are then doesn't wake tonight."
      },
      {
        "id": "innkeeper",
        "name": "Innkeeper",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "The previously protected and drunk players lose those markers. The Innkeeper points to two players. Those players are protected. One is drunk.",
        "reminders": [
          "Safe",
          "Safe",
          "Drunk"
        ],
        "setup": false,
        "ability": "Each night*, choose 2 players: they can't die tonight, but 1 is drunk until dusk."
      },
      {
        "id": "gambler",
        "name": "Gambler",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "The Gambler points to a player, and a character on their sheet. If incorrect, the Gambler dies.",
        "reminders": [
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, choose a player & guess their character: if you guess wrong, you die."
      },
      {
        "id": "gossip",
        "name": "Gossip",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If the Gossip's public statement was true: Choose a player not protected from dying tonight. That player dies.",
        "reminders": [
          "Dead"
        ],
        "setup": false,
        "ability": "Each day, you may make a public statement. Tonight, if it was true, a player dies."
      },
      {
        "id": "courtier",
        "name": "Courtier",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "The Courtier either shows a 'no' head signal, or points to a character on the sheet. If the Courtier used their ability: If that character is in play, that player is drunk.",
        "otherNightReminder": "Reduce the remaining number of days the marked player is poisoned. If the Courtier has not yet used their ability: The Courtier either shows a 'no' head signal, or points to a character on the sheet. If the Courtier used their ability: If that character is in play, that player is drunk.",
        "reminders": [
          "Drunk 1",
          "Drunk 2",
          "Drunk 3",
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, at night, choose a character: they are drunk for 3 nights & 3 days."
      },
      {
        "id": "professor",
        "name": "Professor",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If the Professor has not used their ability: The Professor either shakes their head no, or points to a player. If that player is a Townsfolk, they are now alive.",
        "reminders": [
          "Alive",
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, at night*, choose a dead player: if they are a Townsfolk, they are resurrected."
      },
      {
        "id": "minstrel",
        "name": "Minstrel",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Everyone Is Drunk"
        ],
        "setup": false,
        "ability": "When a Minion dies by execution, all other players (except Travellers) are drunk until dusk tomorrow."
      },
      {
        "id": "tealady",
        "name": "Tea Lady",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Cannot Die",
          "Cannot Die"
        ],
        "setup": false,
        "ability": "If both your alive neighbors are good, they can't die."
      },
      {
        "id": "pacifist",
        "name": "Pacifist",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Executed good players might not die."
      },
      {
        "id": "fool",
        "name": "Fool",
        "edition": "bmr",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "The 1st time you die, you don't."
      },
      {
        "id": "goon",
        "name": "Goon",
        "edition": "bmr",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Drunk"
        ],
        "setup": false,
        "ability": "Each night, the 1st player to choose you with their ability is drunk until dusk. You become their alignment."
      },
      {
        "id": "lunatic",
        "name": "Lunatic",
        "edition": "bmr",
        "team": "outsider",
        "firstNightReminder": "If 7 or more players: Show the Lunatic a number of arbitrary 'Minions', players equal to the number of Minions in play. Show 3 character tokens of arbitrary good characters. If the token received by the Lunatic is a Demon that would wake tonight: Allow the Lunatic to do the Demon actions. Place their 'attack' markers. Wake the Demon. Show the Demon's real character token. Show them the Lunatic player. If the Lunatic attacked players: Show the real demon each marked player. Remove any Lunatic 'attack' markers.",
        "otherNightReminder": "Allow the Lunatic to do the actions of the Demon. Place their 'attack' markers. If the Lunatic selected players: Wake the Demon. Show the 'attack' marker, then point to each marked player. Remove any Lunatic 'attack' markers.",
        "reminders": [
          "Chosen",
          "Chosen",
          "Chosen"
        ],
        "setup": false,
        "ability": "You think you are a Demon, but you are not. The Demon knows who you are & who you choose at night."
      },
      {
        "id": "tinker",
        "name": "Tinker",
        "edition": "bmr",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "The Tinker might die.",
        "reminders": [
          "Dead"
        ],
        "setup": false,
        "ability": "You might die at any time."
      },
      {
        "id": "moonchild",
        "name": "Moonchild",
        "edition": "bmr",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "If the Moonchild used their ability to target a player today: If that player is good, they die.",
        "reminders": [
          "Dead"
        ],
        "setup": false,
        "ability": "When you learn that you died, publicly choose 1 alive player. Tonight, if it was a good player, they die."
      },
      {
        "id": "godfather",
        "name": "Godfather",
        "edition": "bmr",
        "team": "minion",
        "firstNightReminder": "Show each of the Outsider tokens in play.",
        "otherNightReminder": "If an Outsider died today: The Godfather points to a player. That player dies.",
        "reminders": [
          "Died Today",
          "Dead"
        ],
        "setup": true,
        "ability": "You start knowing which Outsiders are in play. If 1 died today, choose a player tonight: they die. [−1 or +1 Outsider]"
      },
      {
        "id": "devilsadvocate",
        "name": "Devil's Advocate",
        "edition": "bmr",
        "team": "minion",
        "firstNightReminder": "The Devil's Advocate points to a living player. That player survives execution tomorrow.",
        "otherNightReminder": "The Devil's Advocate points to a living player, different from the previous night. That player survives execution tomorrow.",
        "reminders": [
          "Survives Execution"
        ],
        "setup": false,
        "ability": "Each night, choose a living player (different to last night): if executed tomorrow, they don't die."
      },
      {
        "id": "assassin",
        "name": "Assassin",
        "edition": "bmr",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "If the Assassin has not yet used their ability: The Assassin either shows the 'no' head signal, or points to a player. That player dies.",
        "reminders": [
          "Dead",
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, at night*, choose a player: they die, even if for some reason they could not."
      },
      {
        "id": "mastermind",
        "name": "Mastermind",
        "edition": "bmr",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If the Demon dies by execution (ending the game), play for 1 more day. If a player is then executed, their team loses."
      },
      {
        "id": "zombuul",
        "name": "Zombuul",
        "edition": "bmr",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "If no-one died during the day: The Zombuul points to a player. That player dies.",
        "reminders": [
          "Died Today",
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, if no-one died today, choose a player: they die. The 1st time you die, you live but register as dead."
      },
      {
        "id": "pukka",
        "name": "Pukka",
        "edition": "bmr",
        "team": "demon",
        "firstNightReminder": "The Pukka points to a player. That player is poisoned.",
        "otherNightReminder": "The Pukka points to a player. That player is poisoned. The previously poisoned player dies.",
        "reminders": [
          "Poisoned",
          "Poisoned",
          "Dead"
        ],
        "setup": false,
        "ability": "Each night, choose a player: they are poisoned. The previously poisoned player dies then becomes healthy."
      },
      {
        "id": "shabaloth",
        "name": "Shabaloth",
        "edition": "bmr",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "One player that the Shabaloth chose the previous night might be resurrected. The Shabaloth points to two players. Those players die.",
        "reminders": [
          "Dead",
          "Dead",
          "Alive"
        ],
        "setup": false,
        "ability": "Each night*, choose 2 players: they die. A dead player you chose last night might be regurgitated."
      },
      {
        "id": "po",
        "name": "Po",
        "edition": "bmr",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "If the Po chose no-one the previous night: The Po points to three players. Otherwise: The Po either shows the 'no' head signal , or points to a player. Chosen players die",
        "reminders": [
          "Dead",
          "Dead",
          "Dead",
          "3 Attacks"
        ],
        "setup": false,
        "ability": "Each night*, you may choose a player: they die. If your last choice was no-one, choose 3 players tonight."
      },
      {
        "id": "apprentice",
        "name": "Apprentice",
        "edition": "bmr",
        "team": "traveller",
        "firstNightReminder": "Show the Apprentice the 'You are' card, then a Townsfolk or Minion token. In the Grimoire, replace the Apprentice token with that character token, and put the Apprentice's 'Is The Apprentice' reminder by that character token.",
        "otherNightReminder": "",
        "reminders": [
          "Is The Apprentice"
        ],
        "setup": false,
        "ability": "On your 1st night, you gain a Townsfolk ability (if good), or a Minion ability (if evil)."
      },
      {
        "id": "matron",
        "name": "Matron",
        "edition": "bmr",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Each day, you may choose up to 3 sets of 2 players to swap seats. Players may not leave their seats to talk in private."
      },
      {
        "id": "judge",
        "name": "Judge",
        "edition": "bmr",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, if another player nominated, you may choose to force the current execution to pass or fail."
      },
      {
        "id": "voudon",
        "name": "Voudon",
        "edition": "bmr",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Only you & the dead can vote. They don't need a vote token to do so. A 50% majority isn't required."
      },
      {
        "id": "bishop",
        "name": "Bishop",
        "edition": "bmr",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Nominate Good",
          "Nominate Evil"
        ],
        "setup": false,
        "ability": "Only the Storyteller can nominate. At least 1 opposing player must be nominated each day."
      },
      {
        "id": "clockmaker",
        "name": "Clockmaker",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "Show the hand signal for the number (1, 2, 3, etc.) of places from Demon to closest Minion.",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "You start knowing how many steps from the Demon to its nearest Minion."
      },
      {
        "id": "dreamer",
        "name": "Dreamer",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "The Dreamer points to a player. Show 1 good and 1 evil character token; one of these is correct.",
        "otherNightReminder": "The Dreamer points to a player. Show 1 good and 1 evil character token; one of these is correct.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, choose a player (not yourself or Travellers): you learn 1 good & 1 evil character, 1 of which is correct."
      },
      {
        "id": "snakecharmer",
        "name": "Snake Charmer",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "The Snake Charmer points to a player. If that player is the Demon: swap the Demon and Snake Charmer character and alignments. Wake each player to inform them of their new role and alignment. The new Snake Charmer is poisoned.",
        "otherNightReminder": "The Snake Charmer points to a player. If that player is the Demon: swap the Demon and Snake Charmer character and alignments. Wake each player to inform them of their new role and alignment. The new Snake Charmer is poisoned.",
        "reminders": [
          "Poisoned"
        ],
        "setup": false,
        "ability": "Each night, choose an alive player: a chosen Demon swaps characters & alignments with you & is then poisoned."
      },
      {
        "id": "mathematician",
        "name": "Mathematician",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "Show the hand signal for the number (0, 1, 2, etc.) of players whose ability malfunctioned due to other abilities.",
        "otherNightReminder": "Show the hand signal for the number (0, 1, 2, etc.) of players whose ability malfunctioned due to other abilities.",
        "reminders": [
          "Abnormal",
          "Abnormal",
          "Abnormal",
          "Abnormal",
          "Abnormal"
        ],
        "setup": false,
        "ability": "Each night, you learn how many players' abilities worked abnormally (since dawn) due to another character's ability."
      },
      {
        "id": "flowergirl",
        "name": "Flowergirl",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "Nod 'yes' or shake head 'no' for whether the Demon voted today. Place the 'Demon Not Voted' marker (remove 'Demon Voted', if any).",
        "reminders": [
          "Demon Voted",
          "Demon Not Voted"
        ],
        "setup": false,
        "ability": "Each night*, you learn if a Demon voted today."
      },
      {
        "id": "towncrier",
        "name": "Town Crier",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "Nod 'yes' or shake head 'no' for whether a Minion nominated today. Place the 'Minion Not Nominated' marker (remove 'Minion Nominated', if any).",
        "reminders": [
          "Minion Nominated",
          "Minions Not Nominated"
        ],
        "setup": false,
        "ability": "Each night*, you learn if a Minion nominated today."
      },
      {
        "id": "oracle",
        "name": "Oracle",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "Show the hand signal for the number (0, 1, 2, etc.) of dead evil players.",
        "reminders": [],
        "setup": false,
        "ability": "Each night*, you learn how many dead players are evil."
      },
      {
        "id": "savant",
        "name": "Savant",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Each day, you may visit the Storyteller to learn 2 things in private: 1 is true & 1 is false."
      },
      {
        "id": "seamstress",
        "name": "Seamstress",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "The Seamstress either shows a 'no' head signal, or points to two other players. If the Seamstress chose players , nod 'yes' or shake 'no' for whether they are of same alignment.",
        "otherNightReminder": "If the Seamstress has not yet used their ability: the Seamstress either shows a 'no' head signal, or points to two other players. If the Seamstress chose players , nod 'yes' or shake 'no' for whether they are of same alignment.",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, at night, choose 2 players (not yourself): you learn if they are the same alignment."
      },
      {
        "id": "philosopher",
        "name": "Philosopher",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "The Philosopher either shows a 'no' head signal, or points to a good character on their sheet. If they chose a character: Swap the out-of-play character token with the Philosopher token and add the 'Is The Philosopher' reminder. If the character is in play, place the drunk marker by that player.",
        "otherNightReminder": "If the Philosopher has not used their ability: the Philosopher either shows a 'no' head signal, or points to a good character on their sheet. If they chose a character: Swap the out-of-play character token with the Philosopher token and add the 'Is The Philosopher' reminder. If the character is in play, place the drunk marker by that player.",
        "reminders": [
          "Drunk",
          "Is The Philosopher"
        ],
        "setup": false,
        "ability": "Once per game, at night, choose a good character: gain that ability. If this character is in play, they are drunk."
      },
      {
        "id": "artist",
        "name": "Artist",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, during the day, privately ask the Storyteller any yes/no question."
      },
      {
        "id": "juggler",
        "name": "Juggler",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If today was the Juggler's first day: Show the hand signal for the number (0, 1, 2, etc.) of 'Correct' markers. Remove markers.",
        "reminders": [
          "Correct",
          "Correct",
          "Correct",
          "Correct",
          "Correct"
        ],
        "setup": false,
        "ability": "On your 1st day, publicly guess up to 5 players' characters. That night, you learn how many you got correct."
      },
      {
        "id": "sage",
        "name": "Sage",
        "edition": "snv",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If the Sage was killed by a Demon: Point to two players, one of which is that Demon.",
        "reminders": [],
        "setup": false,
        "ability": "If the Demon kills you, you learn that it is 1 of 2 players."
      },
      {
        "id": "mutant",
        "name": "Mutant",
        "edition": "snv",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If you are \"mad\" about being an Outsider, you might be executed."
      },
      {
        "id": "barber",
        "name": "Barber",
        "edition": "snv",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "If the Barber died today: Wake the Demon. Show the 'This character selected you' card, then Barber token. The Demon either shows a 'no' head signal, or points to 2 players. If they chose players: Swap the character tokens. Wake each player. Show 'You are', then their new character token.",
        "reminders": [
          "Haircuts Tonight"
        ],
        "setup": false,
        "ability": "If you died today or tonight, the Demon may choose 2 players (not another Demon) to swap characters."
      },
      {
        "id": "sweetheart",
        "name": "Sweetheart",
        "edition": "snv",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "Choose a player that is drunk.",
        "reminders": [
          "Drunk"
        ],
        "setup": false,
        "ability": "When you die, 1 player is drunk from now on."
      },
      {
        "id": "klutz",
        "name": "Klutz",
        "edition": "snv",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "When you learn that you died, publicly choose 1 alive player: if they are evil, your team loses."
      },
      {
        "id": "witch",
        "name": "Witch",
        "edition": "snv",
        "team": "minion",
        "firstNightReminder": "The Witch points to a player. If that player nominates tomorrow they die immediately.",
        "otherNightReminder": "If there are 4 or more players alive: The Witch points to a player. If that player nominates tomorrow they die immediately.",
        "reminders": [
          "Cursed"
        ],
        "setup": false,
        "ability": "Each night, choose a player: if they nominate tomorrow, they die. If just 3 players live, you lose this ability."
      },
      {
        "id": "cerenovus",
        "name": "Cerenovus",
        "edition": "snv",
        "team": "minion",
        "firstNightReminder": "The Cerenovus points to a player, then to a character on their sheet. Wake that player. Show the 'This character selected you' card, then the Cerenovus token. Show the selected character token. If the player is not mad about being that character tomorrow, they can be executed.",
        "otherNightReminder": "The Cerenovus points to a player, then to a character on their sheet. Wake that player. Show the 'This character selected you' card, then the Cerenovus token. Show the selected character token. If the player is not mad about being that character tomorrow, they can be executed.",
        "reminders": [
          "Mad"
        ],
        "setup": false,
        "ability": "Each night, choose a player & a good character: they are \"mad\" they are this character tomorrow, or might be executed."
      },
      {
        "id": "pithag",
        "name": "Pit-Hag",
        "edition": "snv",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "The Pit-Hag points to a player and a character on the sheet. If this character is not in play, wake that player and show them the 'You are' card and the relevant character token. If the character is in play, nothing happens.",
        "reminders": [],
        "setup": false,
        "ability": "Each night*, choose a player & a character they become (if not in play). If a Demon is made, deaths tonight are arbitrary."
      },
      {
        "id": "eviltwin",
        "name": "Evil Twin",
        "edition": "snv",
        "team": "minion",
        "firstNightReminder": "Wake the Evil Twin and their twin. Confirm that they have acknowledged each other. Point to the Evil Twin. Show their Evil Twin token to the twin player. Point to the twin. Show their character token to the Evil Twin player.",
        "otherNightReminder": "",
        "reminders": [
          "Twin"
        ],
        "setup": false,
        "ability": "You & an opposing player know each other. If the good player is executed, evil wins. Good can't win if you both live."
      },
      {
        "id": "fanggu",
        "name": "Fang Gu",
        "edition": "snv",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The Fang Gu points to a player. That player dies. Or, if that player was an Outsider and there are no other Fang Gu in play: The Fang Gu dies instead of the chosen player. The chosen player is now an evil Fang Gu. Wake the new Fang Gu. Show the 'You are' card, then the Fang Gu token. Show the 'You are' card, then the thumb-down 'evil' hand sign.",
        "reminders": [
          "Dead",
          "Once"
        ],
        "setup": true,
        "ability": "Each night*, choose a player: they die. The 1st Outsider this kills becomes an evil Fang Gu & you die instead. [+1 Outsider]"
      },
      {
        "id": "vigormortis",
        "name": "Vigormortis",
        "edition": "snv",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The Vigormortis points to a player. That player dies. If a Minion, they keep their ability and one of their Townsfolk neighbors is poisoned.",
        "reminders": [
          "Dead",
          "Has Ability",
          "Poisoned",
          "Has Ability",
          "Poisoned",
          "Has Ability",
          "Poisoned"
        ],
        "setup": true,
        "ability": "Each night*, choose a player: they die. Minions you kill keep their ability & poison 1 Townsfolk neighbor. [−1 Outsider]"
      },
      {
        "id": "nodashii",
        "name": "No Dashii",
        "edition": "snv",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The No Dashii points to a player. That player dies.",
        "reminders": [
          "Dead",
          "Poisoned",
          "Poisoned"
        ],
        "setup": false,
        "ability": "Each night*, choose a player: they die. Your 2 Townsfolk neighbors are poisoned."
      },
      {
        "id": "vortox",
        "name": "Vortox",
        "edition": "snv",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The Vortox points to a player. That player dies.",
        "reminders": [
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, choose a player: they die. Townsfolk abilities yield false info. Each day, if no-one is executed, evil wins."
      },
      {
        "id": "barista",
        "name": "Barista",
        "edition": "snv",
        "team": "traveller",
        "firstNightReminder": "Choose a player, wake them and tell them which Barista power is affecting them. Treat them accordingly (sober/healthy/true info or activate their ability twice).",
        "otherNightReminder": "Choose a player, wake them and tell them which Barista power is affecting them. Treat them accordingly (sober/healthy/true info or activate their ability twice).",
        "reminders": [
          "Sober & Healthy",
          "Acts Twice",
          "?",
          "?"
        ],
        "setup": false,
        "ability": "Each night, until dusk, 1) a player becomes sober, healthy & gets true info, or 2) their ability works twice. They learn which."
      },
      {
        "id": "harlot",
        "name": "Harlot",
        "edition": "snv",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "The Harlot points at any player. Then, put the Harlot to sleep. Wake the chosen player, show them the 'This character selected you' token, then the Harlot token. That player either nods their head yes or shakes their head no. If they nodded their head yes, wake the Harlot and show them the chosen player's character token. Then, you may decide that both players die.",
        "reminders": [
          "Dead",
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, choose a living player: if they agree, you learn their character, but you both might die."
      },
      {
        "id": "butcher",
        "name": "Butcher",
        "edition": "snv",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Each day, after the 1st execution, you may nominate again."
      },
      {
        "id": "deviant",
        "name": "Deviant",
        "edition": "snv",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If you were funny today, you cannot die by exile."
      },
      {
        "id": "bonecollector",
        "name": "Bone Collector",
        "edition": "snv",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "The Bone Collector either shakes their head no or points at any dead player. If they pointed at any dead player, put the Bone Collector's 'Has Ability' reminder by the chosen player's character token. (They may need to be woken tonight to use it.)",
        "reminders": [
          "No Ability",
          "Has Ability"
        ],
        "setup": false,
        "ability": "Once per game, at night*, choose a dead player: they regain their ability until dusk."
      },
      {
        "id": "steward",
        "name": "Steward",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Point to the marked player.",
        "otherNightReminder": "",
        "reminders": [
          "Know"
        ],
        "setup": false,
        "ability": "You start knowing 1 good player."
      },
      {
        "id": "knight",
        "name": "Knight",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Point to the 2 marked players.",
        "otherNightReminder": "",
        "reminders": [
          "Know",
          "Know"
        ],
        "setup": false,
        "ability": "You start knowing 2 players that are not the Demon."
      },
      {
        "id": "noble",
        "name": "Noble",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Point to 3 players including one evil player, in no particular order.",
        "otherNightReminder": "",
        "reminders": [
          "Know",
          "Know",
          "Know"
        ],
        "setup": false,
        "ability": "You start knowing 3 players, 1 and only 1 of which is evil."
      },
      {
        "id": "shugenja",
        "name": "Shugenja",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "If the closest evil player is in a clockwise direction, point your finger horizontally in that direction. If the closest evil player is in an anti-clockwise direction, point your finger horizontally in that direction. If the two closest evil players are equidistant, point your finger horizontally in either direction.",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "You start knowing if your closest evil player is clockwise or anti-clockwise. If equidistant, this info is arbitrary."
      },
      {
        "id": "bountyhunter",
        "name": "Bounty Hunter",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Point to 1 evil player. Wake the townsfolk who is evil and show them the 'You are' card and the thumbs down evil sign.",
        "otherNightReminder": "If the known evil player has died, point to another evil player. ",
        "reminders": [
          "Known"
        ],
        "setup": false,
        "ability": "You start knowing 1 evil player. If the player you know dies, you learn another evil player tonight. [1 Townsfolk is evil]"
      },
      {
        "id": "pixie",
        "name": "Pixie",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Show the Pixie 1 in-play Townsfolk character token.",
        "otherNightReminder": "",
        "reminders": [
          "Mad",
          "Has Ability"
        ],
        "setup": false,
        "ability": "You start knowing 1 in-play Townsfolk. If you were mad that you were this character, you gain their ability when they die."
      },
      {
        "id": "highpriestess",
        "name": "High Priestess",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Point to a player.",
        "otherNightReminder": "Point to a player.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, learn which player the Storyteller believes you should talk to most."
      },
      {
        "id": "balloonist",
        "name": "Balloonist",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Choose a character type. Point to a player whose character is of that type. Place the Balloonist's 'Know' reminder next to that player.",
        "otherNightReminder": "Choose a player whose character type does not match the type from the previous night. Move the Balloonist's 'Know' reminder next to that player.",
        "reminders": [
          "Know"
        ],
        "setup": true,
        "ability": "Each night, you learn a player of a different character type than last night. [+0 or +1 Outsider]"
      },
      {
        "id": "general",
        "name": "General",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Show the General thumbs up for good winning, thumbs down for evil winning or thumb to the side for neither.",
        "otherNightReminder": "Show the General thumbs up for good winning, thumbs down for evil winning or thumb to the side for neither.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, you learn which alignment the Storyteller believes is winning: good, evil, or neither."
      },
      {
        "id": "preacher",
        "name": "Preacher",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "The Preacher chooses a player. If a Minion is chosen, wake the Minion and show the 'This character selected you' card and then the Preacher token.",
        "otherNightReminder": "The Preacher chooses a player. If a Minion is chosen, wake the Minion and show the 'This character selected you' card and then the Preacher token.",
        "reminders": [
          "No Ability",
          "No Ability",
          "No Ability"
        ],
        "setup": false,
        "ability": "Each night, choose a player: a Minion, if chosen, learns this. All chosen Minions have no ability."
      },
      {
        "id": "villageidiot",
        "name": "Village Idiot",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "The Village Idiot points to a player; give a thumbs up if that player is good or a thumbs down if that player is evil.",
        "otherNightReminder": "The Village Idiot points to a player; give a thumbs up if that player is good or a thumbs down if that player is evil.",
        "reminders": [
          "Drunk"
        ],
        "setup": true,
        "ability": "Each night, choose a player: you learn their alignment. [+0 to +2 Village Idiots. 1 of the extras is drunk]"
      },
      {
        "id": "king",
        "name": "King",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Wake the Demon, show them the 'This character selected you' card, show the King token and point to the King player.",
        "otherNightReminder": "If the dead equal or outnumber the living, show the King a character token of a living player.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, if the dead equal or outnumber the living, you learn 1 alive character. The Demon knows you are the King."
      },
      {
        "id": "cultleader",
        "name": "Cult Leader",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "The cult leader might change alignment. If so, show them the thumbs up good signal or the thumbs down evil signal accordingly.",
        "otherNightReminder": "The cult leader might change alignment. If so, show them the thumbs up good signal or the thumbs down evil signal accordingly.",
        "reminders": [],
        "setup": false,
        "ability": "Each night, you become the alignment of an alive neighbor. If all good players choose to join your cult, your team wins."
      },
      {
        "id": "acrobat",
        "name": "Acrobat",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "The Acrobat points to a player. If that player is or becomes drunk or poisoned tonight, the Acrobat dies.",
        "reminders": [
          "Chosen",
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, choose a player: if they are or become drunk or poisoned tonight, you die."
      },
      {
        "id": "lycanthrope",
        "name": "Lycanthrope",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "The Lycanthrope points to a living player: if good, they die and the Demon does not kill tonight.",
        "reminders": [
          "Faux Paw",
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, choose an alive player. If good, they die & the Demon doesn't kill tonight. One good player registers as evil."
      },
      {
        "id": "alsaahir",
        "name": "Alsaahir",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Each day, if you publicly guess which players are Minion(s) and which are Demon(s), good wins."
      },
      {
        "id": "engineer",
        "name": "Engineer",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "The Engineer shows a 'no' head signal, or points to a Demon or points to the relevant number of Minions. If the Engineer chose characters, replace the Demon or Minions with the choices, then wake the relevant players and show them the You are card and the relevant character tokens.",
        "otherNightReminder": "The Engineer shows a 'no' head signal, or points to a Demon or points to the relevant number of Minions. If the Engineer chose characters, replace the Demon or Minions with the choices, then wake the relevant players and show them the 'You are' card and the relevant character tokens.",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, at night, choose which Minions or which Demon is in play."
      },
      {
        "id": "nightwatchman",
        "name": "Nightwatchman",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "The Nightwatchman may point to a player. Wake that player, show the 'This character selected you' card and the Nightwatchman token, then point to the Nightwatchman player.",
        "otherNightReminder": "The Nightwatchman may point to a player. Wake that player, show the 'This character selected you' card and the Nightwatchman token, then point to the Nightwatchman player.",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, at night, choose a player: they learn you are the Nightwatchman."
      },
      {
        "id": "huntsman",
        "name": "Huntsman",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "The Huntsman shakes their head 'no' or points to a player. If they point to the Damsel, wake that player, show the 'You are' card and a not-in-play character token.",
        "otherNightReminder": "The Huntsman shakes their head 'no' or points to a player. If they point to the Damsel, wake that player, show the 'You are' card and a not-in-play character token.",
        "reminders": [
          "No Ability"
        ],
        "setup": true,
        "ability": "Once per game, at night, choose a living player: the Damsel, if chosen, becomes a not-in-play Townsfolk. [+the Damsel]"
      },
      {
        "id": "fisherman",
        "name": "Fisherman",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "No Ability"
        ],
        "setup": false,
        "ability": "Once per game, during the day, visit the Storyteller for some advice to help your team win."
      },
      {
        "id": "alchemist",
        "name": "Alchemist",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Show the Alchemist the Minion token of the ability they have",
        "otherNightReminder": "",
        "reminders": [],
        "remindersGlobal": [
          "Is The Alchemist"
        ],
        "setup": false,
        "ability": "You have a Minion ability. When using this, the Storyteller may prompt you to choose differently."
      },
      {
        "id": "cannibal",
        "name": "Cannibal",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "Mark the recently killed executee with the 'Lunch' token. If they are good, the Cannibal will wake tonight when the executee would have woken. If they are evil, mark the Cannibal as 'Poisoned'. The Storyteller may pretend that the Cannibal has any ability.",
        "reminders": [
          "Poisoned",
          "Lunch"
        ],
        "setup": false,
        "ability": "You have the ability of the recently killed executee. If they are evil, you are poisoned until a good player dies by execution."
      },
      {
        "id": "amnesiac",
        "name": "Amnesiac",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Decide the Amnesiac's entire ability. If the Amnesiac's ability causes them to wake tonight: Wake the Amnesiac and run their ability.",
        "otherNightReminder": "If the Amnesiac's ability causes them to wake tonight: Wake the Amnesiac and run their ability.",
        "reminders": [
          "?",
          "?",
          "?"
        ],
        "setup": false,
        "ability": "You do not know what your ability is. Each day, privately guess what it is: you learn how accurate you are."
      },
      {
        "id": "farmer",
        "name": "Farmer",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If a Farmer died tonight, choose another good player and make them the Farmer. Wake this player, and show them the 'You are' card and the Farmer character token.",
        "reminders": [],
        "setup": false,
        "ability": "When you die at night, an alive good player becomes a Farmer."
      },
      {
        "id": "choirboy",
        "name": "Choirboy",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If the King was killed by the Demon, wake the Choirboy and point to the Demon player.",
        "reminders": [],
        "setup": true,
        "ability": "If the Demon kills the King, you learn which player is the Demon. [+the King]"
      },
      {
        "id": "banshee",
        "name": "Banshee",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "If the Banshee was killed by the Demon, announce that the Banshee has died.",
        "reminders": [
          "Has Ability"
        ],
        "setup": false,
        "ability": "If the Demon kills you, all players learn this. From now on, you may nominate twice per day and vote twice per nomination."
      },
      {
        "id": "magician",
        "name": "Magician",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "The Demon thinks you are a Minion. Minions think you are a Demon."
      },
      {
        "id": "poppygrower",
        "name": "Poppy Grower",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "Do not inform the Demon/Minions who each other are",
        "otherNightReminder": "If the Poppy Grower has died, show the Minions/Demon who each other are.",
        "reminders": [
          "Evil Wakes"
        ],
        "setup": false,
        "ability": "Minions & Demons do not know each other. If you die, they learn who each other are that night."
      },
      {
        "id": "atheist",
        "name": "Atheist",
        "edition": "",
        "team": "townsfolk",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": true,
        "ability": "The Storyteller can break the game rules, and if executed, good wins, even if you are dead. [No evil characters]"
      },
      {
        "id": "ogre",
        "name": "Ogre",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "The Ogre points to a player (not themselves) and becomes their alignment.",
        "otherNightReminder": "",
        "reminders": [
          "Friend"
        ],
        "setup": false,
        "ability": "On your 1st night, choose a player (not yourself): you become their alignment (you don't know which) even if drunk or poisoned."
      },
      {
        "id": "golem",
        "name": "Golem",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "May Not Nominate"
        ],
        "setup": false,
        "ability": "You may only nominate once per game. When you do, if the nominee is not the Demon, they die."
      },
      {
        "id": "plaguedoctor",
        "name": "Plague Doctor",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Storyteller Ability"
        ],
        "setup": false,
        "ability": "When you die, the Storyteller gains a Minion ability."
      },
      {
        "id": "hatter",
        "name": "Hatter",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "Wake the Minions and Demon. Each player either shakes their head no or points to another character of the same type as their current character. If a second player would end up with the same character as another player, shake your head no and gesture for them to choose again. Change each player to the character they chose.",
        "reminders": [
          "Tea Party Tonight"
        ],
        "setup": false,
        "ability": "If you died today or tonight, the Minion & Demon players may choose new Minion & Demon characters to be."
      },
      {
        "id": "politician",
        "name": "Politician",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If you were the player most responsible for your team losing, you change alignment & win, even if dead."
      },
      {
        "id": "zealot",
        "name": "Zealot",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If there are 5 or more players alive, you must vote for every nomination."
      },
      {
        "id": "damsel",
        "name": "Damsel",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "Wake all the Minions, show them the 'This character selected you' card and the Damsel token.",
        "otherNightReminder": "If selected by the Huntsman, wake the Damsel, show 'You are' card and a not-in-play Townsfolk token.",
        "reminders": [
          "Guess Used"
        ],
        "setup": false,
        "ability": "All Minions know a Damsel is in play. If a Minion publicly guesses you (once), your team loses."
      },
      {
        "id": "snitch",
        "name": "Snitch",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "After Minion info wake each Minion and show them three not-in-play character tokens. These may be the same or different to each other and the ones shown to the Demon.",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Each Minion gets 3 bluffs."
      },
      {
        "id": "heretic",
        "name": "Heretic",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Whoever wins, loses & whoever loses, wins, even if you are dead."
      },
      {
        "id": "puzzlemaster",
        "name": "Puzzlemaster",
        "edition": "",
        "team": "outsider",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Drunk",
          "Guess Used"
        ],
        "setup": false,
        "ability": "1 player is drunk, even if you die. If you guess (once) who it is, learn the Demon player, but guess wrong & get false info."
      },
      {
        "id": "mezepheles",
        "name": "Mezepheles",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Show the Mezepheles their secret word.",
        "otherNightReminder": "Wake the 1st good player that said the Mezepheles' secret word and show them the 'You are' card and the thumbs down evil signal.",
        "reminders": [
          "Turns Evil",
          "No Ability"
        ],
        "setup": false,
        "ability": "You start knowing a secret word. The 1st good player to say this word becomes evil that night."
      },
      {
        "id": "harpy",
        "name": "Harpy",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "The Harpy points to two players. Wake the 1st player the Harpy pointed to, show them the 'This character has selected you' card, show them the Harpy token, then point at the 2nd player the Harpy pointed to.",
        "otherNightReminder": "The Harpy points to two players. Wake the 1st player the Harpy pointed to, show them the 'This character has selected you' card, show them the Harpy token, then point at the 2nd player the Harpy pointed to.",
        "reminders": [
          "Mad",
          "2nd"
        ],
        "setup": false,
        "ability": "Each night, choose 2 players: tomorrow, the 1st player is mad that the 2nd is evil, or one or both might die."
      },
      {
        "id": "fearmonger",
        "name": "Fearmonger",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "The Fearmonger points to a player. Place the Fear token next to that player and announce that a new player has been selected with the Fearmonger ability.",
        "otherNightReminder": "The Fearmonger points to a player. If different from the previous night, place the Fear token next to that player and announce that a new player has been selected with the Fearmonger ability.",
        "reminders": [
          "Fear"
        ],
        "setup": false,
        "ability": "Each night, choose a player: if you nominate & execute them, their team loses. All players know if you choose a new player."
      },
      {
        "id": "psychopath",
        "name": "Psychopath",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Each day, before nominations, you may publicly choose a player: they die. If executed, you only die if you lose roshambo."
      },
      {
        "id": "wizard",
        "name": "Wizard",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "If the Wizard's wish requires actions at night, run these.",
        "otherNightReminder": "If the Wizard's wish requires actions at night, run these.",
        "reminders": [
          "?",
          "?"
        ],
        "setup": false,
        "ability": "Once per game, choose to make a wish. If granted, it might have a price & leave a clue as to its nature."
      },
      {
        "id": "widow",
        "name": "Widow",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Show the Grimoire to the Widow for as long as they need. The Widow points to a player. That player is poisoned. Wake a good player. Show the 'These characters are in play' card, then the Widow character token.",
        "otherNightReminder": "",
        "reminders": [
          "Poisoned",
          "Knows"
        ],
        "setup": false,
        "ability": "On your 1st night, look at the Grimoire & choose a player: they are poisoned. 1 good player knows a Widow is in play."
      },
      {
        "id": "xaan",
        "name": "Xaan",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Add the Xaan's 'Night 1' reminder to the Grimoire. If X is 1, add the 'X' reminder.",
        "otherNightReminder": "Change the Xaan's night reminder to the relevant night. If tonight is night X, add the 'X' reminder.",
        "reminders": [
          "Night 1",
          "Night 2",
          "Night 3",
          "X"
        ],
        "setup": true,
        "ability": "On night X, all Townsfolk are poisoned until dusk. [X Outsiders]"
      },
      {
        "id": "marionette",
        "name": "Marionette",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Select one of the good players next to the Demon and place the 'Is The Marionette' reminder token. Wake the Demon and show them the Marionette.",
        "otherNightReminder": "",
        "reminders": [],
        "remindersGlobal": [
          "Is The Marionette"
        ],
        "setup": true,
        "ability": "You think you are a good character, but you are not. The Demon knows who you are. [You neighbor the Demon]"
      },
      {
        "id": "summoner",
        "name": "Summoner",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Show the 'These characters are not in play' card. Show 3 character tokens of good characters not in play.",
        "otherNightReminder": "If it is the 3rd night, wake the Summoner. They point to a player and a Demon on the character sheet. That player becomes that Demon.",
        "reminders": [
          "Night 1",
          "Night 2",
          "Night 3"
        ],
        "setup": true,
        "ability": "You get 3 bluffs. On the 3rd night, choose a player: they become an evil Demon of your choice. [No Demon]"
      },
      {
        "id": "goblin",
        "name": "Goblin",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Claimed"
        ],
        "setup": false,
        "ability": "If you publicly claim to be the Goblin when nominated & are executed that day, your team wins."
      },
      {
        "id": "boomdandy",
        "name": "Boomdandy",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "If you are executed, all but 3 players die. After a 10 to 1 countdown, the player with the most players pointing at them, dies."
      },
      {
        "id": "vizier",
        "name": "Vizier",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Announce the Vizier player.",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "All players know you are the Vizier. You cannot die during the day. If good voted, you may choose to execute immediately."
      },
      {
        "id": "organgrinder",
        "name": "Organ Grinder",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Wake the Organ Grinder. If they give the 'yes' head signal, mark them Drunk. If they give the 'no' head signal, remove the Drunk reminder.",
        "otherNightReminder": "Wake the Organ Grinder. If they give the 'yes' head signal, mark them Drunk. If they give the 'no' head signal, remove the Drunk reminder.",
        "reminders": [
          "About To Die",
          "Drunk"
        ],
        "setup": false,
        "ability": "All players keep their eyes closed when voting and the vote tally is secret. Each night, choose if you are drunk until dusk."
      },
      {
        "id": "boffin",
        "name": "Boffin",
        "edition": "",
        "team": "minion",
        "firstNightReminder": "Wake the Boffin and show them the token of the good ability the Demon has. Put the Boffin back to sleep. Wake the Demon, show the Boffin token, then show the token of the good ability the Demon has.",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "The Demon (even if drunk or poisoned) has a not-in-play good character's ability. You both know which."
      },
      {
        "id": "yaggababble",
        "name": "Yaggababble",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "Show the Yaggababble their secret phrase.",
        "otherNightReminder": "Choose a number of players up to the total number of times the Yaggababble publicly said their secret phrase today, those players die.",
        "reminders": [
          "Dead",
          "Dead",
          "Dead"
        ],
        "setup": false,
        "ability": "You start knowing a secret phrase. For each time you said it publicly today, a player might die."
      },
      {
        "id": "lilmonsta",
        "name": "Lil' Monsta",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "Wake all Minions together. Allow them to vote by pointing at who they want to babysit Lil' Monsta.",
        "otherNightReminder": "Wake all Minions together. Allow them to vote by pointing at who they want to babysit Lil' Monsta. The Storyteller may choose a player, that player dies.",
        "reminders": [],
        "remindersGlobal": [
          "Is The Demon",
          "Dead"
        ],
        "setup": true,
        "ability": "Each night, Minions choose who babysits Lil' Monsta & \"is the Demon\". Each night*, a player might die. [+1 Minion]"
      },
      {
        "id": "kazali",
        "name": "Kazali",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "The Kazali points to a player and a Minion on the character sheet. They do this for as many Minions as should be in play. Change those players' tokens to the chosen Minion tokens. Wake those players, show them the 'You Are' card, the Minions they have become, and a thumbs down.",
        "otherNightReminder": "The Kazali points to a player. That player dies",
        "reminders": [
          "Dead"
        ],
        "setup": true,
        "ability": "Each night*, choose a player: they die. [You choose which players are which Minions. -? to +? Outsiders]"
      },
      {
        "id": "ojo",
        "name": "Ojo",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The Ojo points to a character on the sheet. If it is in play, that player dies. If it is not in play, the Storyteller chooses who dies instead.",
        "reminders": [
          "Dead"
        ],
        "setup": false,
        "ability": "Each night*, choose a character: they die. If they are not in play, the Storyteller chooses who dies."
      },
      {
        "id": "alhadikhia",
        "name": "Al-Hadikhia",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The Al-Hadikhia might choose 3 players. If so, announce the first player, wake them to nod yes to live or shake head no to die, kill or resurrect accordingly, then put to sleep and announce the next player. If all 3 are alive after this, all 3 die.",
        "reminders": [
          "1",
          "2",
          "3"
        ],
        "setup": false,
        "ability": "Each night*, you may choose 3 players (all players learn who): each silently chooses to live or die, but if all live, all die."
      },
      {
        "id": "legion",
        "name": "Legion",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "The Storyteller may choose a player, that player dies.",
        "reminders": [
          "Dead",
          "About To Die"
        ],
        "setup": true,
        "ability": "Each night*, a player might die. Executions fail if only evil voted. You register as a Minion too. [Most players are Legion]"
      },
      {
        "id": "lordoftyphon",
        "name": "Lord of Typhon",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "Wake the players on either side of the Demon. Show them the 'You Are' card, the token of the Minion they now are, and a thumbs down to indicate they are evil.",
        "otherNightReminder": "The Lord of Typhon points to a player. That player dies.",
        "reminders": [
          "Dead"
        ],
        "setup": true,
        "ability": "Each night*, choose a player: they die. [Evil characters are in a line. You are in the middle. +1 Minion. -? to +? Outsiders]"
      },
      {
        "id": "lleech",
        "name": "Lleech",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "The Lleech points to a player. Place the Poisoned reminder token.",
        "otherNightReminder": "The Lleech points to a player. That player dies.",
        "reminders": [
          "Dead",
          "Poisoned"
        ],
        "setup": false,
        "ability": "Each night*, choose a player: they die. You start by choosing a player: they are poisoned. You die if & only if they are dead."
      },
      {
        "id": "leviathan",
        "name": "Leviathan",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "Place the Leviathan 'Day 1' marker. Announce 'The Leviathan is in play. This is Day 1.'",
        "otherNightReminder": "Change the Leviathan Day reminder for the next day.",
        "reminders": [
          "Day 1",
          "Day 2",
          "Day 3",
          "Day 4",
          "Day 5",
          "Good Player Executed"
        ],
        "setup": false,
        "ability": "If more than 1 good player is executed, evil wins. All players know you are in play. After day 5, evil wins."
      },
      {
        "id": "riot",
        "name": "Riot",
        "edition": "",
        "team": "demon",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Night 1",
          "Night 2",
          "Night 3"
        ],
        "setup": false,
        "ability": "On day 3, Minions become Riot & nominees die but nominate an alive player immediately. This must happen."
      },
      {
        "id": "gangster",
        "name": "Gangster",
        "edition": "",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [],
        "setup": false,
        "ability": "Once per day, you may choose to kill an alive neighbor, if your other alive neighbor agrees."
      },
      {
        "id": "gnome",
        "name": "Gnome",
        "edition": "",
        "team": "traveller",
        "firstNightReminder": "",
        "otherNightReminder": "",
        "reminders": [
          "Amigo"
        ],
        "setup": false,
        "ability": "All players start knowing a player of your alignment. You may choose to kill anyone who nominates them."
      }
    ]
    """