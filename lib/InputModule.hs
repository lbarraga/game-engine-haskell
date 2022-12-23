module InputModule where

import TypeModule
import GameModule
import LevelChooserModule
import Graphics.Gloss.Interface.IO.Game

-- -----------------------------------------------------------------
-- Deze module zorgt ervoor dat input correct wordt afgehandeld en 
-- dat de engine tussen zijn drie staten kan vloeien (zoals te zien
-- is op de afbeelding in het verslag)
-- -----------------------------------------------------------------
--
-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- | Entry point van de module
handleEngineInput ::  Event -> EngineState -> IO EngineState
handleEngineInput  ev (Playing game)     = return $ handleGameInput ev game
handleEngineInput  ev Won                = handleWinScreenInput ev
handleEngineInput  ev (LevelChooser sel) = handleLevelChooserInput ev sel

-- | Input handling op het winscherm
handleWinScreenInput :: Event -> IO EngineState
handleWinScreenInput ev
  | isKey KeySpace ev  = LevelChooser <$> initLevelSelectorIO
handleWinScreenInput _ = return Won

-- | input haandling in het level selector scherm
handleLevelChooserInput :: Event -> LevelSelector -> IO EngineState
handleLevelChooserInput ev
  | isKey KeyUp    ev = return . LevelChooser . moveLevelSelector U
  | isKey KeyDown  ev = return . LevelChooser . moveLevelSelector D
  | isKey KeySpace ev = fmap Playing . chooseLevelFile 
handleLevelChooserInput _ = return . LevelChooser

-- | input handling terwijl er gespeeld wordt. 
-- | Dit zal input delegeren nr de speler input 
-- | of naar de action pnel input naargelang de 
-- | actionpanel actief is of niet.
handleGameInput :: Event -> Game -> EngineState
handleGameInput ev game 
  | panelStatus == On  = Playing $ handleActionPanelInput ev game
  | otherwise          = handlePlayerInput ev game
  where panelStatus = (status . panelMode) game

-- | speler input handling (om de speler te verplaatsen)
handlePlayerInput :: Event -> Game -> EngineState
handlePlayerInput ev
  | isKey KeyDown  ev = handleDirectionInput D
  | isKey KeyUp    ev = handleDirectionInput U
  | isKey KeyRight ev = handleDirectionInput R
  | isKey KeyLeft  ev = handleDirectionInput L
handlePlayerInput _   = Playing

-- | selector verplaatsen in het action paneel
handleActionPanelInput :: Event -> Game -> Game
handleActionPanelInput ev 
  | isKey KeyDown  ev = moveActionsSelectorGame D 
  | isKey KeyUp    ev = moveActionsSelectorGame U 
  | isKey KeySpace ev = togglePanelModeOffGame . selectAction
handleActionPanelInput _ = id 

-- | Een speler wil in een bepaalde richting bewegen.
-- | Hier zijn de mogelijke scenario's en hoe ze afgehandeld moeten worden
handleDirectionInput :: Dir -> Game -> EngineState
handleDirectionInput dir game
  | hasActionInDirGame dir game              = Playing $ togglePanelModeOnGame (getActionFromDirectionGame dir game) game
  | canMoveGame dir game                     = Playing $ movePlayerGame dir game
  | hasEndGame dir game && hasNextLevel game = Playing $ nextLevel game
  | hasEndGame dir game                      = Won
  | otherwise = Playing game

