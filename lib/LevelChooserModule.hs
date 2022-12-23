module LevelChooserModule where

import TypeModule
import ParserModule (parseGameFile)
import GameModule (setBackup)

import System.Directory (listDirectory)
import Data.List (sort)
import Control.Monad.IO.Class (MonadIO(liftIO))

-- | Geen een lijst terug van alle levels in een director
listDirectoryLevels :: String -> IO [String]
listDirectoryLevels = (sort <$>) . (filter (endsWith ".txt") <$>) . listDirectory 

-- | Of een string met een andere string eindigd
endsWith :: String -> String -> Bool
endsWith with = (==with) . reverse . take (length with) . reverse

-- | De initiele LevelSelector
initLevelSelectorIO :: IO LevelSelector
initLevelSelectorIO = do
    lvlString <- listDirectoryLevels "levels"
    return (LevelSelector lvlString 0)

-- | Verplaats de Selector van de levelSelector in een bepaalde richting
moveLevelSelector :: Dir -> LevelSelector -> LevelSelector
moveLevelSelector U = onLevelSelectorPos (subtract 1)
moveLevelSelector D = onLevelSelectorPos (+ 1)
moveLevelSelector _ = error "richting niet ondersteund."

-- | Voer een functie uit op de selector van de leverlselector
onLevelSelectorPos :: (Int -> Int) -> LevelSelector -> LevelSelector
onLevelSelectorPos f ls = ls{levelSelectorPos = ((`mod` length (levelFiles ls)) . f) (levelSelectorPos ls)}

-- | Kies het level onder de levelSelector
chooseLevelFile :: LevelSelector -> IO Game
chooseLevelFile levelSelector = do
    game <- (parseGameFile . ("levels/" ++) . getSelectedFile) levelSelector
    return $ setBackup game

getSelectedFile :: LevelSelector -> String
getSelectedFile ls = levelFiles ls !! levelSelectorPos ls
