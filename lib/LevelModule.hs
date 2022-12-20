module LevelModule where

import TypeModule
import PlayerModule -- TODO weg
import Data.Maybe (listToMaybe, fromJust)
import Data.List (elemIndices)


-- -------------------------------------------------
--
-- -------------------------------------------------

wall, empty, start, end :: Char
wall  = '*'
empty = '.'
start = 's'
end   = 'e'

-- -------------------------------------------------
--
-- -------------------------------------------------

findPlayer :: Level -> (Int, Int)
findPlayer l = fromJust $ listToMaybe indexen
    where indexen = [ (x, y) | (y,line) <- zip [0..] (layout l), x <- elemIndices 's' line ]

replaceInLayout :: (Int, Int) -> Char -> Layout -> Layout
replaceInLayout (x, y) c l = replaceAtIndex y replacedRow l
    where replacedRow = replaceAtIndex x c (l !! y)

movePlayerInLayout :: Dir -> Level -> Layout
movePlayerInLayout dir lvl = (replaceInLayout newPos 's' . replaceInLayout oldPos '.') (layout lvl) -- TODO tekens in vars steken
    where oldPos    = findPlayer lvl
          newPos    = addPositions oldPos (getMoveDelta dir)

movePlayer :: Dir -> Level -> Level
movePlayer dir lvl = lvl{layout = movePlayerInLayout dir lvl}

-- -------------------------------------------------
--
-- -------------------------------------------------

canMove :: Dir -> Level -> Bool
canMove dir level = canMoveTo (getNewPlayerPos dir level) level

getNewPlayerPos :: Dir -> Level -> (Int, Int)
getNewPlayerPos dir lvl = addPositions (findPlayer lvl) (getMoveDelta dir)

anyOnPosition :: [(Int, Int) -> Level -> Bool] -> (Int, Int) -> Level -> Bool
anyOnPosition hasFunctions pos level = any (\f -> f pos level) hasFunctions

canMoveTo :: (Int, Int) -> Level -> Bool
canMoveTo pos lvl = not $ anyOnPosition [hasEntity, hasItem, hasWall] pos lvl

-- -------------------------------------------------
--
-- -------------------------------------------------

hasEntity :: (Int, Int) -> Level -> Bool
hasEntity = hasLevelObject entities entityX entityY 

hasItem :: (Int, Int) -> Level -> Bool
hasItem = hasLevelObject items itemX itemY

hasWall :: (Int, Int) -> Level -> Bool
hasWall (x, y) = (==wall) . (!! x) . (!! y) . layout 

hasLevelObject :: (Level -> [a]) -> (a -> Int) -> (a -> Int) -> (Int, Int) -> Level -> Bool
hasLevelObject getObjs getX getY pos = not . null . getLevelObjectsOnPos getObjs getX getY pos 

hasAction :: (Int, Int) -> Level -> Bool
hasAction = anyOnPosition [hasItem, hasEntity]

hasActionInDir :: Dir -> Level -> Bool
hasActionInDir pos lvl = hasAction (getNewPlayerPos pos lvl) lvl 

-- -------------------------------------------------
--
-- -------------------------------------------------

getActionsFromEntity :: (Int, Int) -> Level -> [ConditionalAction]
getActionsFromEntity pos = getFieldFromObj (getEntityOnPos pos) entityActions

getActionsFromItem :: (Int, Int) -> Level -> [ConditionalAction]
getActionsFromItem pos = getFieldFromObj (getItemOnPos pos) itemActions

getActionFromDirection :: Dir -> Level -> [ConditionalAction]
getActionFromDirection dir lvl
  | hasItem pos lvl   = getActionsFromItem pos lvl
  | hasEntity pos lvl = getActionsFromEntity pos lvl
  | otherwise = error $ "No actions on position " ++ show pos
  where pos = getNewPlayerPos dir lvl

getFieldFromObj ::  (Level -> a) -> (a -> b) -> Level -> b
getFieldFromObj getObj getField = getField . getObj

getItemOnPos :: (Int, Int) -> Level -> Item
getItemOnPos pos = head . getLevelObjectsOnPos items itemX itemY pos

getEntityOnPos :: (Int, Int) -> Level -> Entity
getEntityOnPos pos = head . getLevelObjectsOnPos entities entityX entityY pos

getLevelObjectsOnPos :: (Level -> [a]) -> (a -> Int) -> (a -> Int) -> (Int, Int) -> Level -> [a]
getLevelObjectsOnPos getObjs getX getY pos = filter (isOnPosition pos getX getY) . getObjs

isOnPosition :: (Int, Int) -> (a -> Int) -> (a -> Int) -> a -> Bool
isOnPosition (x, y) getX getY levelObj = x == getX levelObj && y == getY levelObj

-- -------------------------------------------------
--
-- -------------------------------------------------

onEntity :: (Entity -> Entity) -> Entity -> Level -> Level
onEntity f entity lvl = lvl{entities = replaceObjInList entity (f entity) (entities lvl)}

onEntities :: ([Entity] -> [Entity]) -> Level -> Level
onEntities f lvl = lvl{entities = f (entities lvl)}

onItems :: ([Item] -> [Item]) -> Level -> Level
onItems f lvl = lvl{items = f (items lvl)}

decreaseHp :: Item -> Entity ->  Level -> Level
decreaseHp playerWeapon entity
  | eHp - damage <= 0 = removeEntityFromLevel entity
  | otherwise         = onEntity (onEntityHp (subtract damage)) entity
  where damage = itemValue playerWeapon
        eHp = fromJust $ entityHp entity

removeFromItemLevel :: Item -> Level -> Level
removeFromItemLevel item = onItems (filter (/= item)) 

removeEntityFromLevel :: Entity -> Level -> Level
removeEntityFromLevel entity = onEntities (filter (/= entity)) 

-- -------------------------------------------------
--
-- -------------------------------------------------

-- -------------------------------------------------
--
-- -------------------------------------------------

left, right, up, down :: (Int, Int)
left  = (-1, 0)
right = ( 1, 0)
up    = ( 0, 1)
down  = ( 0,-1)

getMoveDelta :: Dir -> (Int, Int)
getMoveDelta L = left
getMoveDelta R = right
getMoveDelta U = up 
getMoveDelta D = down

addPositions :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPositions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
