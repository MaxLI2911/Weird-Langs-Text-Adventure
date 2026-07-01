-- Kill & Cook, by Maksim Lubentsov, Krzysztak Wojtaszko, Mykhailo Marfenko

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (when)
import Control.Monad.State.Strict
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO (hFlush, stdout)

-- Domain

data Room = EntranceRoom | DustyRoom | WellRoom | FireRoom | StorageRoom | BreweryRoom | DwarfsRoom | RuneRoom | MerchantRoom 
  deriving (Eq, Ord, Enum, Bounded)

instance Show Room where
  show = \case
    EntranceRoom -> "room_a"
    DustyRoom -> "room_b"
    WellRoom -> "room_c"
    FireRoom -> "room_d"
    StorageRoom -> "storage_room"
    BreweryRoom -> "brewery_room"
    DwarfsRoom -> "dwarfs_room"
    RuneRoom -> "rune_room"
    MerchantRoom -> "dwarfs_room"

data Dir = N | S | E | W deriving (Eq, Ord, Show)

dirName :: Dir -> String
dirName = \case
  N -> "n"
  S -> "s"
  E -> "e"
  W -> "w"

data Item = Sword | GoldCoin | Bucket | Water | Treasure | GoblinMeat | CooledAsh | Salt | Malt | Beer | DwarfMeat | Mushrooms | LegendaryDish
  deriving (Eq, Ord, Show)

data GoblinState = GoblinAlive | GoblinDead | GoblinBribed deriving (Eq, Show)
data DwarfState = DwarfAlive | DwarfDead | DwarfBribed deriving (Eq, Show)
data ChestState = ChestClosed | ChestOpen deriving (Eq, Show)
data FireState = FireAlive | FireDead deriving (Eq, Show)

maxHP :: Int
maxHP = 3

data GameState = GameState
  { gsRoom :: Room, 
  gsHP :: Int, 
  gsInv :: S.Set Item, 
  gsRoomItems :: M.Map Room (S.Set Item), 
  gsGoblin :: GoblinState, 
  gsChest :: ChestState, 
  gsStorageChest :: ChestState, 
  gsFire :: FireState, 
  gsDwarf :: DwarfState,
  gsRuneDoorOpen :: Bool,
  gsBucketOnHead :: Bool, 
  gsGameOver :: Bool
  } 
  deriving (Eq, Show)

type GameM = StateT GameState IO

-- Level

paths :: M.Map (Room, Dir) Room
paths = M.fromList
  [ ((EntranceRoom, E), DustyRoom)
  , ((DustyRoom, W), EntranceRoom)
  , ((DustyRoom, E), WellRoom)
  , ((WellRoom, W), DustyRoom)
  , ((WellRoom, E), FireRoom)
  , ((FireRoom, W), WellRoom)
  , ((DustyRoom, N), StorageRoom)
  , ((StorageRoom, S), DustyRoom)
  , ((StorageRoom, N), BreweryRoom)
  , ((BreweryRoom, S), StorageRoom)
  , ((StorageRoom, E), DwarfsRoom)
  , ((DwarfsRoom, W), StorageRoom)
  , ((DwarfsRoom, S), WellRoom)
  , ((WellRoom, N), DwarfsRoom)
  , ((RuneRoom, W), DwarfsRoom)
  , ((MerchantRoom, W), RuneRoom)
  , ((DwarfsRoom, E), RuneRoom)
  , ((MerchantRoom, W), RuneRoom)
  , ((RuneRoom, E), MerchantRoom) ]

moveDir :: Dir -> Room -> Maybe Room
moveDir d r = M.lookup (r, d) paths

availableDirs :: Room -> [Dir]
availableDirs r = [ d | d <- [N,S,E,W], M.member (r,d) paths ]

-- Init / Reset

emptyRooms :: M.Map Room (S.Set Item)
emptyRooms = M.fromList [(r, S.empty) | r <- [minBound .. maxBound]]

initialState :: GameState
initialState = GameState
  { gsRoom = EntranceRoom, 
  gsHP = maxHP, 
  gsInv = S.empty, 
  gsRoomItems = M.insert RuneRoom (S.singleton Mushrooms) emptyRooms,
  gsGoblin = GoblinAlive, 
  gsDwarf = DwarfAlive,
  gsRuneDoorOpen = False,
  gsChest = ChestClosed, 
  gsStorageChest = ChestClosed, 
  gsFire = FireAlive, 
  gsBucketOnHead = True, 
  gsGameOver = False }

resetGame :: GameM ()
resetGame = put initialState >> instructions >> status >> look

-- Utilities

say :: String -> GameM ()
say = liftIO . putStrLn

prompt :: GameM String
prompt = liftIO $ putStr "> " >> hFlush stdout >> getLine

norm :: String -> String
norm = map toLower . filter (/= '.')

wordsN :: String -> [String]
wordsN = words . norm

roomItemsAt :: Room -> GameState -> S.Set Item
roomItemsAt r st = M.findWithDefault S.empty r (gsRoomItems st)

setRoomItemsAt :: Room -> S.Set Item -> GameState -> GameState
setRoomItemsAt r items st = st { gsRoomItems = M.insert r items (gsRoomItems st) }

addItemToRoom :: Room -> Item -> GameState -> GameState
addItemToRoom r it st =
  let items = roomItemsAt r st
  in setRoomItemsAt r (S.insert it items) st

removeItemFromRoom :: Room -> Item -> GameState -> GameState
removeItemFromRoom r it st =
  let items = roomItemsAt r st
  in setRoomItemsAt r (S.delete it items) st

hasInv :: Item -> GameState -> Bool
hasInv it st = S.member it (gsInv st)

addInv :: Item -> GameState -> GameState
addInv it st = st { gsInv = S.insert it (gsInv st) }

delInv :: Item -> GameState -> GameState
delInv it st = st { gsInv = S.delete it (gsInv st) }

damage :: Int -> GameM ()
damage dmg = do
  st <- get
  let newHP = gsHP st - dmg
  say $ "You take damage: " ++ show dmg
  if newHP <= 0
    then do
      put st { gsHP = 0, gsGameOver = True }
      say "HP left: 0"
      die
    else do
      put st { gsHP = newHP }
      say $ "HP left: " ++ show newHP

heal :: Int -> GameM ()
heal amt = do
  st <- get
  let newHP = min maxHP (gsHP st + amt)
  put st { gsHP = newHP }
  say $ "You reagain " ++ show amt ++ " HP."
  say $ "Current HP: " ++ show newHP

status :: GameM ()
status = do
  st <- get
  say $ "Your HP: " ++ show (gsHP st)
  if S.null (gsInv st)
    then pure ()
    else say $ "Inventory: " ++ unwords (map itemName (S.toList (gsInv st)))

die :: GameM ()
die = do
  say "You are dead loser! Better luck next time"
  say "The game is over, GG. Enter the \"halt.\" command."

-- Look / Describe / Exits

look :: GameM ()
look = do
  st <- get
  describe (gsRoom st)
  say ""
  noticeObjects
  say ""
  exits

exits :: GameM ()
exits = do
  st <- get
  let ds = availableDirs (gsRoom st)
  if null ds
    then say "There are no exits from here."
    else do
      say "You can go: "
      mapM_ (\d -> say ("  - " ++ dirName d)) ds

describe :: Room -> GameM ()
describe r = do
  st <- get
  case r of
    EntranceRoom -> do
      say "You are in a first room."
      case gsGoblin st of
        GoblinAlive -> say "An ugly goblin is here!"
        GoblinDead -> say "The body of a slain goblin is on the floor."
        GoblinBribed -> say "The room is empty now."

    DustyRoom -> do
      say "This room is dusty."
      case gsChest st of
        ChestClosed -> say "There is a closed wooden chest."
        ChestOpen -> say "There is an open wooden chest."

    WellRoom -> do
      say "A room with a well in the middle."
      say "The water looks surprisingly clean."

    FireRoom -> do
      say "It's incredibly hot in here!"
      case gsFire st of
        FireAlive -> say "A swirling fire elemental crackles and pops in the corner."
        FireDead -> say "A pile of cool ash sits in the corner."

    StorageRoom -> do
      say "A dusty storage room filled with old barrels and crates."
      case gsStorageChest st of
        ChestClosed -> say "There is a closed small wooden chest in the corner."
        ChestOpen   -> say "There is an open small wooden chest in the corner."

    BreweryRoom -> do
      say "This room smells of hops and fermentation."
      say "A complex brewing apparatus stands in the center, connected to copper pipes."
      say "The apparatus looks ready for use."


    DwarfsRoom -> do
      say "A narrow stone corridor."
      case gsDwarf st of
        DwarfAlive -> do
           say "A stout dwarf blocks the EAST exit."
           say "\"None shall pass without a tribute!\" he grumbles."
        DwarfDead -> say "The dead body of the dwarf lies here."
        DwarfBribed -> say "A dwarf is sleeping loudly against the wall."

    RuneRoom -> do
      say "A circular chamber covered in ancient runes."
      say "The walls glow with a soft blue light."
      say "Inscription above the eastern door reads:"
      say "\"Beer is a blessing and a curse\""
      say "Below it, letters are numbered and you can press them:"
      say "B-1, e-2, e-3, r-4, i-5, s-6, a-7, b-8, l-9, e-10, s-11, s-12, i-13, n-14, g-15, a-16, n-17, d-18, a-19, c-20, u-21, r-22, s-23, e-24"
      say "Strange glowing mushrooms grow in the corners."
      if gsRuneDoorOpen st
        then say "The stone door to the EAST is OPEN."
        else say "The stone door to the EAST is CLOSED."

    MerchantRoom -> do
      say "A small chamber illuminated by eerie purple flames."
      say "The walls seem to absorb the light, creating deep shadows."
      say "In the center sits a mysterious figure wrapped in a dark cloak."
      say "His face is concealed by a violet mask that seems to shift in the light."
      say ""
      say "The merchant seems hungry, eyeing you expectantly: \"I could go for a proper meal...\""

noticeObjects :: GameM ()
noticeObjects = do
  st <- get
  let r = gsRoom st
      items = S.toList (roomItemsAt r st)

      features :: [String]
      features = case r of
        EntranceRoom -> if gsGoblin st == GoblinAlive then ["goblin"] else []
        DustyRoom -> ["chest"]
        WellRoom -> ["well"]
        FireRoom -> if gsFire st == FireAlive then ["fire_elemental"] else []
        StorageRoom -> ["wood_chest"]
        BreweryRoom -> ["brewing_apparatus"]
        DwarfsRoom -> if gsDwarf st == DwarfAlive then ["dwarf","locked_door"] else []
        RuneRoom -> ["mushrooms", "final_door"]
        MerchantRoom -> ["merchant"]

      featureLines = [ "There is a " ++ f ++ " here." | f <- features ]
      itemLines = [ "There is a " ++ itemName it ++ " here." | it <- items ]

  mapM_ say (featureLines ++ itemLines)

itemName :: Item -> String
itemName = \case
  Sword -> "sword"
  GoldCoin -> "gold_coin"
  Bucket -> "bucket"
  Water -> "water"
  Treasure -> "treasure"
  GoblinMeat -> "goblin_meat"
  CooledAsh -> "cooled_ash"
  Salt -> "salt"
  Malt -> "malt"
  Beer -> "beer"
  DwarfMeat -> "dwarf_meat"
  Mushrooms -> "mushrooms"
  LegendaryDish -> "legendary_dish"

parseItem :: String -> Maybe Item
parseItem s = case norm s of
  "sword" -> Just Sword
  "gold_coin" -> Just GoldCoin
  "bucket" -> Just Bucket
  "water" -> Just Water
  "treasure" -> Just Treasure
  "goblin_meat" -> Just GoblinMeat
  "cooled_ash" -> Just CooledAsh
  "salt" -> Just Salt
  "malt" -> Just Malt
  "beer" -> Just Beer
  "dwarf_meat" -> Just DwarfMeat
  "mushrooms" -> Just Mushrooms
  "legendary_dish" -> Just LegendaryDish
  _ -> Nothing

-- Actions

takeItem :: Item -> GameM ()
takeItem it = do
  st <- get
  let r = gsRoom st

  -- block taking bucket from goblin's head while goblin alive
  if it == Bucket && r == EntranceRoom && gsGoblin st == GoblinAlive && gsBucketOnHead st
    then say "The goblin won't let you take that from his head."
    else if hasInv it st
      then say "You're already holding it!"
      else
        let itemsHere = roomItemsAt r st
        in if S.member it itemsHere
             then do
               put (addInv it (removeItemFromRoom r it st))
               say "OK."
             else say "I can't take it (I don't see it here)."

dropItem :: Item -> GameM ()
dropItem it = do
  st <- get
  if hasInv it st
    then do
      let r = gsRoom st
      put (delInv it (addItemToRoom r it st))
      say "OK."
    else say "You aren't holding it!"

openChest :: GameM ()
openChest = do
  st <- get
  case gsRoom st of
    DustyRoom ->
      case gsChest st of
        ChestOpen -> say "The chest is already open."
        ChestClosed -> do
          let st'  = st { gsChest = ChestOpen }
              st'' = addItemToRoom DustyRoom Sword (addItemToRoom DustyRoom GoldCoin st')
          put st''
          say "You open the chest."

    StorageRoom ->
      case gsStorageChest st of
        ChestOpen -> say "The wooden chest is already open."
        ChestClosed -> do
          let st'  = st { gsStorageChest = ChestOpen }
              st'' = addItemToRoom StorageRoom Salt (addItemToRoom StorageRoom Malt st')
          put st''
          say "You open the wooden chest. Inside you find bags of salt and malt!"

    _ -> say "You can't open that here!"

attackGoblin :: GameM ()
attackGoblin = do
  st <- get
  if gsRoom st /= EntranceRoom
    then say "You can't attack that!"
    else case gsGoblin st of
      GoblinDead   -> say "The goblin is already dead."
      GoblinBribed -> say "That won't work."
      GoblinAlive ->
        if hasInv Sword st
          then do
            let st1 = st { gsGoblin = GoblinDead, gsBucketOnHead = False }
                st2 = addItemToRoom EntranceRoom Bucket (addItemToRoom EntranceRoom GoblinMeat st1)
            put st2
            say "You slay the goblin."
          else do
            say "You have no weapon! The goblin punches you."
            damage 1

bribeGoblin :: Item -> GameM ()
bribeGoblin it = do
  st <- get
  if gsRoom st /= EntranceRoom || gsGoblin st /= GoblinAlive
    then say "That won't work."
    else if not (hasInv it st)
      then say "That won't work."
      else do
        let st1 = delInv it st
            st2 = st1 { gsGoblin = GoblinBribed, gsBucketOnHead = False }
            st3 = addItemToRoom EntranceRoom Bucket st2
        put st3
        say "The goblin is delighted and scampers off, dropping his bucket."

fillBucket :: GameM ()
fillBucket = do
  st <- get
  if gsRoom st /= WellRoom || not (hasInv Bucket st)
    then say "You need a bucket and to be at the well."
    else if hasInv Water st
      then say "Your bucket is already full."
      else do
        put (addInv Water st)
        say "You fill the bucket with fresh well water."

tossCoin :: GameM ()
tossCoin = do
  st <- get
  if gsRoom st /= WellRoom || not (hasInv GoldCoin st)
    then say "No coin to toss, or you are not at the well."
    else do
      let st1 = delInv GoldCoin st
      put st1
      say "Plop! The coin vanishes. Writing on the wall appears..."
      say "'Meat, salt, and elemental ash, stirred and baked just right'"
      say "A bite of this legendary dish brings flavors pure and bright.”"
      say "(Use cook_meal when you have the right ingedients.)"

splashElemental :: GameM ()
splashElemental = do
  st <- get
  if gsRoom st /= FireRoom
    then say "That won't work."
    else case gsFire st of
      FireDead  -> say "It is already dead."
      FireAlive ->
        if hasInv Water st
          then do
            let st1 = delInv Water st
                st2 = st1 { gsFire = FireDead }
                st3 = addItemToRoom FireRoom CooledAsh st2
            put st3
            say "The elemental dissolves into a cloud of steam."
          else say "You have no water to splash!"

attackElemental :: GameM ()
attackElemental = do
  st <- get
  if gsRoom st /= FireRoom
    then say "You can't attack that!"
    else case gsFire st of
      FireDead -> say "The elemental is just a pile of cool ash."
      FireAlive ->
        if hasInv Sword st
          then do
            put (delInv Sword st)
            say "You swing your sword, but it MELTS instantly in the intense heat!"
            say "The elemental lashes out, burning you."
            damage 1
          else do
            say "You have no weapon! You get too close and are burned."
            damage 1

eatItem :: Item -> GameM ()
eatItem it = do
  st <- get
  case it of
    GoblinMeat ->
      if hasInv GoblinMeat st
        then do
          put (delInv GoblinMeat st)
          say "You cook and eat the goblin meat. Tastes... gamey. You feel a bit better."
          heal 1
        else say "You can't eat that!"
    DwarfMeat ->
      if hasInv DwarfMeat st
        then do
          put (delInv DwarfMeat st)
          say "You butcher the dwarf and attempt to cook his meat..."
          say "This feels... wrong. Very wrong."
          say "The meat smells foul and is saturated with dwarven ale."
          say "You now have cooked dwarf meat. What have you become?"
          say "You take a bite of the dwarf meat..."
          say "It tastes even worse than it smells!"
          say "The meat is so saturated with alcohol it burns your throat."
          say "You feel sick and dizzy from the foul concoction."
          damage 1 
        else say "You don't have that."
    Mushrooms ->
      if hasInv Mushrooms st
        then do
          put (delInv Mushrooms st)
          say "You eat the glowing mushrooms..."
          say "The world starts to swirl around you!"
          say "Colors become brighter, sounds become distorted."
          say "You feel euphoric but completely disoriented."
          say "The runes on the walls seem to dance and whisper secrets..."
          say ""
          say "*** You are experiencing intense hallucinations ***"
          say "The inscription now reads: \"beer is a blessing and a cure\""
          say "The letters seem to float in the air..."
        else say "You don't have them."
    LegendaryDish ->
      if hasInv LegendaryDish st
        then do
           put (delInv LegendaryDish st)
           say "You eat the magical meal. Your body feels completely restored!"
           say "It tastes like victory, starlight, and savory perfection!"
           say "Your wounds knit together instantly."
           heal 100 
        else say "You don't have it."
    _ -> say "You can't eat that!"

brewBeer :: GameM ()
brewBeer = do
  st <- get
  if gsRoom st /= BreweryRoom
    then say "You can't brew anything here! Go to the Brewery Room."
    else do
      let haveWater = hasInv Water st
          haveMalt  = hasInv Malt st
      
      if haveWater && haveMalt
        then do
          let st1 = delInv Water st   
              st2 = delInv Malt st1    
              st3 = addInv Beer st2    
          put st3
          say "You carefully combine water and malt in the brewing apparatus..."
          say "After some bubbling and hissing, you have crafted a fine beer!"
          say "The beer looks refreshing and gives off a pleasant aroma..."
        else do
          say "The machine needs ingredients to work."
          if not haveWater then say "- You need water." else pure ()
          if not haveMalt  then say "- You need malt."  else pure ()

drinkItem :: Item -> GameM ()
drinkItem it = do
  st <- get
  case it of
    Beer ->
      if hasInv Beer st
        then do
          put (delInv Beer st)
          say "You take a long drink of the freshly brewed beer."
          say "It's surprisingly good! You feel refreshed and a bit tipsy..."
          when (gsRoom st == RuneRoom) $ do
             say "It doesn't fit in your head that beer can be a curse."
          heal 2
        else say "You don't have any beer!"
    
    Water -> 
      if hasInv Water st
        then say "You drink some water. Refreshing, but it doesn't heal you."
        else say "You don't have water!"
        
    _ -> say "You can't drink that!"

attackDwarf :: GameM ()
attackDwarf = do
  st <- get
  if gsRoom st /= DwarfsRoom
    then say "The dwarf is not here."
    else case gsDwarf st of
      DwarfDead   -> say "He is already dead."
      DwarfBribed -> say "He is drunk and sleeping. No sport in that."
      DwarfAlive  ->
        if hasInv Sword st
          then do
            say "You swing your sword at the dwarf with all your might!"
            say "*CLANG!* The sword shatters against the dwarf''s steel armor!"
            say "But the force of your blow staggers the dwarf..."
            say "As his armor rings from the impact, he loses balance and falls."
            say "The dwarf crashes to the ground, defeated."
            say "The dwarf drops his battle axe."
            say "Shards of your broken sword scatter across the floor."
            say "However, during the struggle he managed to wound you."
            damage 2 
            
            stAfterHit <- get
            if not (gsGameOver stAfterHit) 
              then do
                let st1 = stAfterHit { gsDwarf = DwarfDead }
                    st2 = addItemToRoom DwarfsRoom DwarfMeat st1
                put st2
              else pure ()
          else do
            say "The dwarf laughs heartily! 'Fool! You dare face me without a proper weapon?'"
            say "With one mighty swing of his battle axe, he cleaves you in two!"
            damage 3

bribeDwarf :: Item -> GameM ()
bribeDwarf it = do
  st <- get
  if gsRoom st /= DwarfsRoom || gsDwarf st /= DwarfAlive
    then say "That won't work."
    else case it of
      Beer -> do
        if hasInv Beer st
          then do
            let st1 = delInv Beer st
                st2 = st1 { gsDwarf = DwarfBribed }
            put st2
            say "You offer the dwarf your freshly brewed beer."
            say "His eyes light up! 'Now that''s a proper drink!.'"
            say "He eagerly takes the beer and starts drinking, completely forgetting about his duty."
          else say "You don't have beer!"
      _ -> say "The dwarf scowls. 'I don''t want that! Bring me proper dwarven drink!'"

pressButton :: String -> GameM ()
pressButton btn = do
  st <- get
  if gsRoom st /= RuneRoom
    then say "There are no buttons here to press."
    else if gsRuneDoorOpen st
      then say "The door is already open. No need to press anything else."
      else case btn of
        "23" -> do
           put st { gsRuneDoorOpen = True }
           say "*CLICK!* The door unlocks!"
           say "The inscription glows brightly: 'The s in curse was the key!'"
           say "The door slowly swings open, revealing the final chamber beyond.'"
           say "You press letter number 23..."
        _ -> do
           say $ "You press button " ++ btn ++ "..."
           say "BZZZT! A magical shock zaps you from the panel!"
           say "A thought enters your mind: 'My brain is too sober for this puzzle. I need a drink.'"
           damage 1

cookMeal :: GameM ()
cookMeal = do
  st <- get
  let hasSalt = hasInv Salt st
      hasAsh  = hasInv CooledAsh st
      
      mainIngredient
        | hasInv GoblinMeat st = Just GoblinMeat
        | hasInv DwarfMeat st  = Just DwarfMeat
        | hasInv Mushrooms st  = Just Mushrooms
        | otherwise            = Nothing

  case (hasSalt, hasAsh, mainIngredient) of
    (True, True, Just item) -> do
      let st1 = delInv Salt st
          st2 = delInv CooledAsh st1
          st3 = delInv item st2 
          st4 = addInv LegendaryDish st3
      put st4
      
      say $ "You toss " ++ itemName item ++ ", salt, and elemental ash into the pot."
      say "The heat from the ash cooks the dish instantly without fire."
      say "The meal smells exquisite!"
      
    _ -> do
      say "You are missing ingredients for the Legendary Dish."
      say "You need: Salt, Cooled Ash, AND one another"

attackMerchant :: GameM ()
attackMerchant = do
  st <- get
  if gsRoom st /= MerchantRoom
    then say "The merchant is not here."
    else do
      say "As you raise your weapon, the merchant doesn't flinch."
      say "The purple flames suddenly flare up!"
      say "A wave of unnatural force throws you against the wall!"
      say "The merchant remains seated, unmoving."
      say "'Violence is the language of the simple-minded...'"
      damage 3 

bribeMerchant :: Item -> GameM ()
bribeMerchant it = do
  st <- get
  if gsRoom st /= MerchantRoom
    then say "That won't work."
    else if not (hasInv it st)
      then say "You aren't holding that."
      else case it of
        LegendaryDish -> do
          put (delInv LegendaryDish st)
          say "You offer the magical meal to the merchant."
          say "He inhales the aroma and smiles: \"Ah, finally some proper food!\""
          say "He waves his hand and a swirl of magical energy envelops you."
          say "You are teleported out of the dungeon. Congratulations! You have won the game!"
          modify' (\s -> s { gsGameOver = True })
        
        _ -> say "The merchant sniffs the air: \"I need a proper meal, not that!\""

-- Help

instructions :: GameM ()
instructions = do
  say ""
  say "Enter commands using standard syntax."
  say "Available commands are:"
  say "start                   -- to start the game."
  say "n  s  e  w               -- to go in that direction."
  say "take ITEM                -- to pick up an object."
  say "drop ITEM                -- to put down an object."
  say "look                     -- to look around you again."
  say "instructions             -- to see this message again."
  say "open chest               -- open the chest"
  say "attack goblin            -- attack goblin"
  say "attack fire_elemental    -- attack fire_elemental"
  say "bribe goblin ITEM         -- bribe goblin with ANY held item"
  say "fill_bucket              -- fill bucket with water at the well"
  say "toss_coin                -- toss a gold coin into the well"
  say "splash fire_elemental     -- splash water on the elemental"
  say "eat goblin_meat           -- a light Dungeon Meshi snack (+1 HP)"
  say "brew                   -- to brew beer (needs water and malt in Brewery)"
  say "drink ITEM               -- drink a beverage"
  say "press N                -- press a button number"
  say "cook_meal              -- cook the legendary dish (needs ingredients)"
  say "status                   -- to see how much hp you have."
  say "exits                    -- list exits"
  say "halt                     -- to end the game and quit."
  say ""

-- Command handling

handle :: [String] -> GameM ()
handle [] = pure ()
handle (cmd:args) = do
  st <- get
  if gsGameOver st && cmd `notElem` ["start","quit","halt"]
    then say "The game is over. Please enter the \"halt.\" command."
    else case cmd of
      "start" -> resetGame
      "look"  -> look
      "status" -> status
      "exits" -> exits
      "help"  -> instructions
      "instructions" -> instructions

      "quit" -> modify' (\s -> s { gsGameOver = True }) >> say "GG!"
      "halt" -> modify' (\s -> s { gsGameOver = True }) >> say "GG!"

      "n" -> go N
      "s" -> go S
      "e" -> go E
      "w" -> go W

      "take" -> case args of
        [x] -> maybe (say "I don't see it here.") takeItem (parseItem x)
        _ -> say "I don't understand."

      "drop" -> case args of
        [x] -> maybe (say "You aren't holding it!") dropItem (parseItem x)
        _ -> say "I don't understand."

      "open" -> case args of
        ["chest"] -> openChest
        _ -> say "You can't open that!"

      "attack" -> case args of
        ["goblin"] -> attackGoblin
        ["fire_elemental"] -> attackElemental
        ["dwarf"] -> attackDwarf  
        ["merchant"] -> attackMerchant
        _ -> say "You can't attack that!"

      "bribe" -> case args of
        ("goblin":itemStr:[]) -> maybe (say "Error") bribeGoblin (parseItem itemStr)
        ("dwarf":itemStr:[]) -> maybe (say "Error") bribeDwarf (parseItem itemStr)
        ("merchant":itemStr:[]) -> maybe (say "Error") bribeMerchant (parseItem itemStr) 
        _ -> say "That won't work."

      "fill_bucket" -> fillBucket
      "toss_coin" -> tossCoin

      "brew" -> brewBeer
      "drink" -> case args of
        [x] -> maybe (say "You can't drink that!") drinkItem (parseItem x)
        _ -> say "I don't understand what you want to drink."

      "splash" -> case args of
        ["fire_elemental"] -> splashElemental
        _ -> say "That won't work."

      "eat" -> case args of
        [x] -> maybe (say "You can't eat that!") eatItem (parseItem x)
        _ -> say "You can't eat that!"

      "cook_meal" -> cookMeal

      "press" -> case args of
        [x] -> pressButton x
        _ -> say "Which button?"

      _ -> say "I don't understand. Type 'instructions'."

  where
    go d = do
      st <- get
      let dwarfBlocked = gsRoom st == DwarfsRoom && d == E && gsDwarf st == DwarfAlive
      
      let runeBlocked = gsRoom st == RuneRoom && d == E && not (gsRuneDoorOpen st)
      
      if dwarfBlocked
        then say "The dwarf blocks your path! You shall not pass!"
        else if runeBlocked
          then say "The door is locked. You need to solve the puzzle to open it."
          else case moveDir d (gsRoom st) of
            Nothing -> say "You can't go that way."
            Just r2 -> put st { gsRoom = r2 } >> look

-- Main loop

gameLoop :: GameM ()
gameLoop = do
  line <- prompt
  let toks = wordsN line
  handle toks
  st <- get
  when (not (norm line `elem` ["quit","halt"])) gameLoop

main :: IO ()
main = do
  putStrLn "Dungeon Meshi mini (Haskell). Type 'start' to begin."
  evalStateT gameLoop initialState