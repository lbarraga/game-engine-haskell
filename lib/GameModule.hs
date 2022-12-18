module GameModule where

import TypeModule
import LevelModule (movePlayer, canMove)
import PlayerModule (replaceAtIndex)

onLevels :: ([Level] -> [Level]) -> Game -> Game
onLevels f g = g {levels = f (levels g)}

getCurrentLevel :: Game -> Level
getCurrentLevel = head . levels

onCurrentLevel :: (Level -> Level) -> Game -> Game
onCurrentLevel f = onLevels (\levels -> replaceAtIndex 0 (f (head levels)) levels)

nextLevel :: Game -> Game
nextLevel = onLevels tail

movePlayerGame :: Dir -> Game -> Game
movePlayerGame dir = onCurrentLevel $ movePlayer dir

canMoveGame :: Dir -> Game -> Bool
canMoveGame dir = canMove dir . getCurrentLevel 
