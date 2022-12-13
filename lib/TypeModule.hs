module TypeModule where

-- | een veld van een object of van een entitie. 
-- | omdat objecten en entities heel veel gemeenschappelijke
-- | velden hebben, is er geen onderscheid gemaakt.
data ObjectField = Id String 
                | X Int
                | Y Int
                | Hp Int
                | Name String
                | Description String     
                | UseTimes UseTime
                | Direction Dir
                | ObjectValue Int
                | Inventory [Object]
                | Actions [Action]
                deriving (Eq, Show)

data Entity = Entity {
    entityId :: String,
    entityX :: Int,
    entityY :: Int,
    entityName :: String, 
    entityDiscription :: String,
    entityDirection :: Maybe Dir,
    entityHp :: Maybe Int,
    entityValue :: Maybe Int, 
    entityActions :: [Action]
} deriving (Eq, Show)

data Item = Item {
    itemId :: String,
    itemX :: Int,
    itemY :: Int,
    itemName :: String,
    itemDescription :: String,
    itemUseTimes :: UseTime,
    itemValue :: Int,
    itemActions :: [Action]
} deriving (Eq, Show)

-- | De richting van een object
data Dir = U | D | L | R deriving (Eq, Show)
data UseTime = Finite Int | Infinite deriving (Eq, Show)

-- | een identifier van een object
type Id = String

-- | Het argument van een functie is ofwel nog een functie (bv. bij not(...)),
-- | ofwel één of meerdere ids
data Arguments = ArgFunction Function | Ids [Id] deriving (Eq, Show)

-- | Een functie heeft een naam en argumenten
data Function = Function {
    name :: String,
    arguments :: Arguments
} deriving (Eq, Show)

-- | een object of entity
type Object = [ObjectField]

-- | een actie is heeft bepaalde condities en een actie 
-- | om uit te voeren wanneer deze condities allemaal waar zijn
data Action = Action {
    conditions :: [Function],
    action :: Function
} deriving (Eq, Show)

-- | Een speler heeft hp en een inventaris.
data Player = Player{hp :: Int, attributes :: [Item]} deriving (Eq, Show)

-- | De level-layout
type Layout = [[Char]]

-- | Een level bestaat uit een layout, 
-- | een lijst van objecten en een lijst van entities.
data Level = Level {
    layout :: Layout, 
    items :: [Item], 
    entities :: [Entity]
} deriving (Eq, Show) 

-- | Een Game heeft een speler en een lijst van levels.
data Game = Game{player :: Player, levels :: [Level]} deriving (Eq, Show)
