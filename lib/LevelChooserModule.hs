module LevelChooserModule where

import TypeModule
import System.Directory (listDirectory)
import Data.List (sort)
import Control.Monad.IO.Class (MonadIO(liftIO))

data LevelSelector = LevelSelector{
    levelFiles :: [String],
    levelSelectorPos :: Int
} deriving (Eq, Show)

listDirectoryLevels :: String -> IO [String]
listDirectoryLevels = (sort <$>) . (filter (endsWith ".txt") <$>) . listDirectory 

endsWith :: String -> String -> Bool
endsWith with = (==with) . reverse . take (length with) . reverse

initLevelSelectorIO :: IO LevelSelector
initLevelSelectorIO = do
    lvlString <- listDirectoryLevels "levels"
    return (LevelSelector lvlString 0)

moveLevelSelector :: Dir -> LevelSelector -> LevelSelector
moveLevelSelector U = onLevelSelectorPos (subtract 1)
moveLevelSelector D = onLevelSelectorPos (+ 1)
moveLevelSelector _ = error "richting niet ondersteund."

onLevelSelectorPos :: (Int -> Int) -> LevelSelector -> LevelSelector
onLevelSelectorPos f ls = ls{levelSelectorPos = ((`mod` length (levelFiles ls)) . f) (levelSelectorPos ls)}
