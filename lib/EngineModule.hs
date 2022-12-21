module EngineModule where

import TypeModule
import LevelChooserModule (LevelSelector (levelFiles, levelSelectorPos))
import ParserModule (parseGameFile)

data EngineState = LevelChooser LevelSelector | Playing Game | Won deriving (Eq, Show) 

chooseLevelFile :: LevelSelector -> Game
chooseLevelFile = parseGameFile . ("levels/" ++) . getSelectedFile

getSelectedFile :: LevelSelector -> String
getSelectedFile ls = levelFiles ls !! levelSelectorPos ls
