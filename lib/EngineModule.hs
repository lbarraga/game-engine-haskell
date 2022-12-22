module EngineModule where

import TypeModule
import LevelChooserModule (LevelSelector (levelFiles, levelSelectorPos))
import ParserModule (parseGameFile)
import GameModule (setBackup)

data EngineState = LevelChooser LevelSelector | Playing Game | Won deriving (Eq, Show) 

chooseLevelFile :: LevelSelector -> IO Game
chooseLevelFile levelSelector = do
    game <- (parseGameFile . ("levels/" ++) . getSelectedFile) levelSelector
    return $ setBackup game

getSelectedFile :: LevelSelector -> String
getSelectedFile ls = levelFiles ls !! levelSelectorPos ls


