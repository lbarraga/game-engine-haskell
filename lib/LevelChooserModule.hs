module LevelChooserModule where

import TypeModule
import GHC.IO (unsafePerformIO) 
import System.Directory (listDirectory)
import Data.List (sort)

data LevelSelector = LevelSelector{
    levelFiles :: [String],
    levelSelectorPos :: Int
} deriving (Eq, Show)

listDirectoryLevels :: String -> [String]
listDirectoryLevels = sort . filter (endsWith ".txt") . unsafePerformIO . listDirectory 

endsWith :: String -> String -> Bool
endsWith with = (==with) . reverse . take (length with) . reverse


initLevelSelector :: LevelSelector
initLevelSelector = LevelSelector (listDirectoryLevels "levels") 0

moveLevelSelector :: Dir -> LevelSelector -> LevelSelector
moveLevelSelector U = onLevelSelectorPos (subtract 1)
moveLevelSelector D = onLevelSelectorPos (+ 1)
moveLevelSelector _ = error "richting niet ondersteund."

onLevelSelectorPos :: (Int -> Int) -> LevelSelector -> LevelSelector
onLevelSelectorPos f ls = ls{levelSelectorPos = ((`mod` length (levelFiles ls)) . f) (levelSelectorPos ls)}
