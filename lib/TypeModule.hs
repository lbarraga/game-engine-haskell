module TypeModule where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

data EngineState = LevelChooser LevelSelector | Playing Game | Won deriving (Eq, Show) 

data LevelSelector = LevelSelector{
    levelFiles :: [String],
    levelSelectorPos :: Int
} deriving (Eq, Show)

-- | Om gedupliceerde code te vermijden,
-- | moeten de gemeenschappelijke attributen van items en entities
-- | op éénzelfde manier opgevraagd worden
class GameObject a where
    getId :: a -> Id
    getX :: a -> X
    getY :: a -> Y
    getName :: a -> String
    getDescription :: a -> String
    getActions :: a -> [ConditionalAction]

-- | een entity in het spel, dit kan een statische 
-- | of actieve entity zijn naargelang hij een hp bezit.
data Entity = Entity {
    entityId :: Id,
    entityX :: X,
    entityY :: Y,
    entityName :: String,
    entityDescription :: String,
    entityDirection :: Maybe Dir,
    entityHp :: Maybe Int,
    entityValue :: Maybe Int,
    entityActions :: [ConditionalAction]
} deriving (Eq, Show)

instance GameObject Entity where 
    getId = entityId
    getX = entityX
    getY = entityY
    getName = entityName
    getDescription = entityDescription
    getActions = entityActions

data Item = Item {
    itemId :: Id,
    itemX :: X,
    itemY :: Y,
    itemName :: String,
    itemDescription :: String, 
    itemUseTimes :: UseTime Int,
    itemValue :: Int,
    itemActions :: [ConditionalAction]
} deriving (Eq, Show)

instance GameObject Item where
    getId = itemId
    getX = itemX
    getY = itemY
    getName = itemName
    getDescription = itemDescription
    getActions = itemActions


-- | De richting van een object
data Dir = U | D | L | R deriving (Eq, Show)
data UseTime a = Finite a | Infinite deriving (Eq, Show)

instance Functor UseTime where
    fmap f Infinite   = Infinite
    fmap f (Finite n) = Finite (f n)

-- | een identifier van een object
type Id = String

-- | Coordinaten
type X = Int
type Y = Int

-- | Het argument van een functie is ofwel nog een functie (bv. bij not(...)),
-- | ofwel één of meerdere ids
data Arguments = ArgFunction Function | Ids [Id] deriving (Eq, Show)

-- | Een functie heeft een naam en argumenten
data Function = Function {
    fName :: String,
    arguments :: Arguments
} deriving (Eq, Show)

-- | om uit te voeren wanneer deze condities allemaal waar zijn
data ConditionalAction = Action {
    conditions :: [Function],
    action :: Function
} deriving (Eq, Show)

-- | Een speler heeft hp en een inventaris.
data Player = Player{hp :: Int, inventory :: [Item]} deriving (Eq, Show)

-- | De level-layout
type Layout = [[Char]]

-- | Een level bestaat uit een layout, 
-- | een lijst van objecten en een lijst van entities.
data Level = Level {
    layout :: Layout, 
    items :: [Item], 
    entities :: [Entity]
} deriving (Eq, Show) 


defaultPanel :: PanelMode
defaultPanel = PanelMode Off 0 [] Nothing 

data PanelStatus = On | Off deriving (Eq, Show)
data PanelMode = PanelMode{
    status :: PanelStatus,
    selectorPos :: Int,
    panelActions :: [ConditionalAction],
    actionEntity :: Maybe Entity
} deriving (Eq, Show)

-- | Een Game heeft een speler en een lijst van levels.
data Game = Game{
    player :: Player, 
    levels :: [Level],
    backup :: Maybe (Player, [Level]), 
    panelMode :: PanelMode
} deriving (Eq, Show)

-- -------------------------------------------------------
-- Om verdere code leesbaarder te maken definieren we 
-- hier een aantal functies die een record unpacken, 
-- een functie toepassen op een van de attributen en terug
-- inpakken. Deze functie beginnen steeds met 'on'
-- -------------------------------------------------------

onEntityHp :: (Int -> Int) -> Entity -> Entity
onEntityHp f e = e{entityHp = f <$> entityHp e}

onItemUseTimes, onItemValue :: (Int -> Int) -> Item -> Item
onItemUseTimes f item = item{itemUseTimes = f <$> itemUseTimes item}
onItemValue    f item = item{itemValue    = f (itemValue item)}

-- --------------------------------------------------------
-- Constanten en functies die veel modules nodig hebben.
-- --------------------------------------------------------

wall, empty, speler, end :: Char
wall   = '*'
empty  = '.'
speler = 's'
end    = 'e'

replaceObjInList :: Eq a => a -> a -> [a] -> [a]
replaceObjInList from to l = replaceAtIndex index to l
    where index = fromJust (elemIndex from l)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index repl l = before ++ [repl] ++ after
    where (before, _:after) = splitAt index l
