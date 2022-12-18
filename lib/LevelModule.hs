module LevelModule where

import TypeModule
import PlayerModule -- TODO weg
import Data.Maybe (listToMaybe, fromJust)
import Data.List (elemIndices)

wall, empty, start, end :: Char
wall  = '*'
empty = '.'
start = 's'
end   = 'e'

findPlayer :: Layout -> (Int, Int)
findPlayer l = fromJust $ listToMaybe indexen
    where indexen = [ (x,y) | (y,line) <- zip [0..] l, x <- elemIndices 's' line ]

replaceInLayout :: (Int, Int) -> Char -> Layout -> Layout
replaceInLayout (x, y) c l = replaceAtIndex y replacedRow l
    where replacedRow = replaceAtIndex x c (l !! y)

movePlayerInLayout :: Dir -> Layout -> Layout
movePlayerInLayout dir layout = (replaceInLayout newPos 's' . replaceInLayout oldPos '.') layout -- TODO tekens in vars steken
    where oldPos    = findPlayer layout
          newPos    = addPositions oldPos (getMoveDelta dir)

movePlayer :: Dir -> Level -> Level
movePlayer dir level@Level{layout = l} = level{layout = movePlayerInLayout dir l}

canMove :: Dir -> Level -> Bool
canMove dir level = canMoveTo newPos level
    where newPos  = addPositions (findPlayer (layout level)) (getMoveDelta dir)

canMoveTo :: (Int, Int) -> Level -> Bool
canMoveTo pos level = not $ any (\f -> f pos level) [hasEntity, hasItem, hasWall]

hasEntity :: (Int, Int) -> Level -> Bool
hasEntity = yeet entities entityX entityY 

hasItem :: (Int, Int) -> Level -> Bool
hasItem = yeet items itemX itemY

hasWall :: (Int, Int) -> Level -> Bool
hasWall (x, y) = (==wall) . (!! x) . (!! y) . layout 

yeet :: (Level -> [a]) -> (a -> Int) -> (a -> Int) -> (Int, Int) -> Level -> Bool
yeet getObjs getX getY pos = any (isOnPosition pos getX getY) . getObjs

isOnPosition :: (Int, Int) -> (a -> Int) -> (a -> Int) -> a -> Bool
isOnPosition (x, y) getX getY levelObj = x == getX levelObj && y == getY levelObj

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
