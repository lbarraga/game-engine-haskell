module LevelModule where

import TypeModule
import Data.Maybe (listToMaybe, fromJust)
import Data.List (elemIndices)

-- -------------------------------------------------
-- Methodes met betrekking tot het bewegen van de 
-- speler. Dit gebeurt door de 's' in de layout te
-- bewegen.
-- -------------------------------------------------

-- | Vind de positie van de 's' in de layout
findPlayer :: Level -> (X, Y)
findPlayer l = fromJust $ listToMaybe indexen
    where indexen = [ (x, y) | (y,line) <- zip [0..] (layout l), x <- elemIndices speler line ]

-- | verwissel een bepaalde positie in de layout met een nieuw karakter
replaceInLayout :: (X, Y) -> Char -> Layout -> Layout
replaceInLayout (x, y) c l = replaceAtIndex y replacedRow l
    where replacedRow = replaceAtIndex x c (l !! y)

-- | verplaats de s in een gegeven richting
movePlayerInLayout :: Dir -> Level -> Layout
movePlayerInLayout dir lvl = (replaceInLayout newPos speler . replaceInLayout oldPos empty) (layout lvl) -- TODO tekens in vars steken
    where oldPos    = findPlayer lvl
          newPos    = addPositions oldPos (getMoveDelta dir)

-- | verplaats de speler
movePlayer :: Dir -> Level -> Level
movePlayer dir lvl = lvl{layout = movePlayerInLayout dir lvl}

-- -------------------------------------------------
--
-- -------------------------------------------------

-- | Of een speler in een gegeven richting kan bewegen
canMove :: Dir -> Level -> Bool
canMove dir level = canMoveTo (getNewPlayerPos dir level) level

-- | Geef de positie van de speler, moest die in een bepaalde richting bewegen
getNewPlayerPos :: Dir -> Level -> (X, Y)
getNewPlayerPos dir lvl = addPositions (findPlayer lvl) (getMoveDelta dir)

-- | evalueert een lijst van functies. Elk element in deze lijst
-- | is een functie die zegt of 'iets' op een positie staat
-- | geeft false terug als ze allemaal tot false evalueren
anyOnPosition :: [(X, Y) -> Level -> Bool] -> (X, Y) -> Level -> Bool
anyOnPosition hasFunctions pos level = any (\f -> f pos level) hasFunctions

-- | De speler kan naar een bepaalde positie gaan, 
-- | als daar geen entity, item of muur is
canMoveTo :: (X, Y) -> Level -> Bool
canMoveTo pos = not . anyOnPosition [hasEntity, hasItem, hasTile wall, hasTile end] pos

-- -------------------------------------------------
--
-- -------------------------------------------------

-- | of er op een bepaalde positie een entity is.
hasEntity :: (X, Y) -> Level -> Bool
hasEntity = hasObjectOnPos entities

-- | of er op een bepaalde positie een item is.
hasItem :: (X, Y) -> Level -> Bool
hasItem = hasObjectOnPos items

hasEndInDir :: Dir -> Level -> Bool
hasEndInDir dir level = hasTile end (getNewPlayerPos dir level) level

-- | Of er op een bepaakde positie een bepaalde tile is (verschillende tiles uit de layout).
hasTile :: Char -> (X, Y) -> Level -> Bool
hasTile tile (x, y) = (==tile) . (!! x) . (!! y) . layout

-- | Gegeven een functie die de objecten uit het level haalt, en een positie,
-- | evalueer True als een van de ge-extraheerde objecten wich op de positie bevind
hasObjectOnPos :: GameObject a => (Level -> [a]) -> (X, Y) -> Level -> Bool  
hasObjectOnPos getObjs pos = not . null . getObjectsOnPos getObjs pos 

-- | Een positie heeft een actie als er een item of een entity is.
hasAction :: (X, Y) -> Level -> Bool
hasAction = anyOnPosition [hasItem, hasEntity]


-- | Of er een actie is in een bepaalde richting gezien vanaf de speler.
hasActionInDir :: Dir -> Level -> Bool
hasActionInDir pos lvl = hasAction (getNewPlayerPos pos lvl) lvl 

-- -------------------------------------------------
--
-- -------------------------------------------------

-- | Haal de acties op van een entity op een bepaalde positie 
getActionsFromEntity :: (X, Y) -> Level -> [ConditionalAction]
getActionsFromEntity pos = getActionsFromObject $ getEntityOnPos pos

-- | Haal de acties op van een item op een bepaalde positie
getActionsFromItem :: (X, Y) -> Level -> [ConditionalAction]
getActionsFromItem pos = getActionsFromObject $ getItemOnPos pos

-- | Haal de acties op van een gameObject op een bepaalde positie
getActionsFromObject :: GameObject a => (Level -> a) -> Level -> [ConditionalAction]
getActionsFromObject getObject = getActions . getObject

-- | Haal de acties op van een item of entity in een gegeven richting 
-- | gezien vanaf de speler.
getActionFromDirection :: Dir -> Level -> ([ConditionalAction], Maybe Entity)
getActionFromDirection dir lvl
  | hasItem pos lvl   = (getActionsFromItem pos lvl, Nothing)
  | hasEntity pos lvl = (getActionsFromEntity pos lvl, Just (getEntityOnPos pos lvl))
  | otherwise = error $ "No actions on position " ++ show pos
  where pos = getNewPlayerPos dir lvl

-- | Geef het item op een bepaalde positie terug
-- | `hasItem` moet altijd eerst op deze positie opgeroepen worden
-- | om zeker te weten dat hier welfegelijk een item ligt
getItemOnPos :: (X, Y) -> Level -> Item
getItemOnPos pos = head . getObjectsOnPos items pos

-- | Geef de entity op de positie terug
getEntityOnPos :: (X, Y) -> Level -> Entity
getEntityOnPos pos = head . getObjectsOnPos entities pos

-- | Gegeven een functie die een lijst van objecten uit een level haalt
-- | geef alle objecten terig die zich op een bepaalde positie bevinden.
getObjectsOnPos :: GameObject a => (Level -> [a]) -> (X, Y) -> Level -> [a]
getObjectsOnPos getObjs pos = filter (isOnPosition pos) . getObjs

-- | of een bepaald gameObject zich op een gegeven positie bevind
isOnPosition :: GameObject a => (X, Y) -> a -> Bool
isOnPosition (x, y) levelObj = x == getX levelObj && y == getY levelObj

-- -------------------------------------------------
-- Hier zijn een aantal functies gedefinieerd die 
-- een record unpacken, een functie toepassen en 
-- deze dan weer inpakken. Dit om leesbaarheid 
-- te verbeteren.
-- -------------------------------------------------

-- | Pas een functie toe op een bepaalde entity in dit level
onEntity :: (Entity -> Entity) -> Entity -> Level -> Level
onEntity f entity = onEntities (replaceObjInList entity (f entity))

-- | Pas een functie toe op de lijst van entities in dit level
onEntities :: ([Entity] -> [Entity]) -> Level -> Level
onEntities f lvl = lvl{entities = f (entities lvl)}

-- | Pas een functie toe op de lijst van items in dit level
onItems :: ([Item] -> [Item]) -> Level -> Level
onItems f lvl = lvl{items = f (items lvl)}

-- -------------------------------------------------
-- Een aantal semantische functies op een level
-- -------------------------------------------------

-- | Gegeven een wapen en een entity, damage de entity
-- | met de kracht van het wapen, en verwijder deze 
-- | entity indien zijn levenspunten onder nul gaan
decreaseHp :: Item -> Entity ->  Level -> Level
decreaseHp spelerWeapon entity
  | eHp - damage <= 0 = removeEntityFromLevel entity
  | otherwise         = onEntity (onEntityHp (subtract damage)) entity
  where damage = itemValue spelerWeapon
        eHp = fromJust $ entityHp entity

-- | verwijder een Item uit het level
removeItemFromLevel :: Item -> Level -> Level
removeItemFromLevel item = onItems (filter (/= item)) 

-- | verwijder een Entity uit het level
removeEntityFromLevel :: Entity -> Level -> Level
removeEntityFromLevel entity = onEntities (filter (/= entity)) 

-- ----------------------------------------------------
-- een aantal hulpFuncties voor het coordinatenSysteem
-- ----------------------------------------------------

left, right, up, down :: (Int, Int)
left  = (-1, 0)
right = ( 1, 0)
up    = ( 0, 1)
down  = ( 0,-1)

getMoveDelta :: Dir -> (X, Y)
getMoveDelta L = left
getMoveDelta R = right
getMoveDelta U = up 
getMoveDelta D = down

addPositions :: (X, Y) -> (X, Y) -> (X, Y)
addPositions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
