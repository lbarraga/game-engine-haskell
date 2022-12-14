module TypeModule where

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

onEntityX, onEntityY, onEntityHp, onEntityValue :: (Int -> Int) -> Entity -> Entity
onEntityX     f e = e{entityX     = f (entityX e)}
onEntityY     f e = e{entityY     = f (entityY e)}
onEntityHp    f e = e{entityHp    = f <$> entityHp e}
onEntityValue f e = e{entityValue = f <$> entityValue e}

data Item = Item {
    itemId :: String,
    itemX :: Int,
    itemY :: Int,
    itemName :: String,
    itemDescription :: String,
    itemUseTimes :: UseTime Int,
    itemValue :: Int,
    itemActions :: [Action]
} deriving (Eq, Show)

onItemX, onItemY, onItemUseTimes, onItemValue :: (Int -> Int) -> Item -> Item
onItemX        f item = item{itemX        = f (itemX item)}
onItemY        f item = item{itemY        = f (itemY item)}
onItemUseTimes f item = item{itemUseTimes = f <$> itemUseTimes item}
onItemValue    f item = item{itemValue    = f (itemValue item)}

-- | De richting van een object
data Dir = U | D | L | R deriving (Eq, Show)
data UseTime a = Finite a | Infinite deriving (Eq, Show)

instance Functor UseTime where
    fmap f Infinite   = Infinite
    fmap f (Finite n) = Finite (f n)

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

-- | om uit te voeren wanneer deze condities allemaal waar zijn
data Action = Action {
    conditions :: [Function],
    action :: Function
} deriving (Eq, Show)

-- | Een speler heeft hp en een inventaris.
data Player = Player{hp :: Int, inventory :: [Item]} deriving (Eq, Show)

onPlayerHp :: (Int -> Int) -> Player -> Player
onPlayerHp f p = p{hp = f (hp p)}

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
