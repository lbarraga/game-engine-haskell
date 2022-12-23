module GameModule where

import TypeModule
import PlayerModule
import LevelModule
import ActionPanelModule
import Data.List (nub)


onLevels :: ([Level] -> [Level]) -> Game -> Game
onLevels f g = g {levels = f (levels g)}

onPlayer :: (Player -> Player) -> Game -> Game
onPlayer f g = g{player = f (player g)}

onCurrentLevel :: (Level -> Level) -> Game -> Game
onCurrentLevel f = onLevels (\levels -> replaceAtIndex 0 (f (head levels)) levels)

onPanelMode :: (PanelMode -> PanelMode) -> Game -> Game
onPanelMode f g = g{panelMode = f (panelMode g)}

-- | Stel het backupVeld van de Game
setBackup :: Game -> Game
setBackup g@Game{levels = levels, player = player} = g{backup = Just (player, levels)}

-- ----------------------------------------------------
--  Een aantal semantische functies in een game
-- ----------------------------------------------------

getCurrentLevel :: Game -> Level
getCurrentLevel = head . levels

-- | Ga naar het volgende Level
nextLevel :: Game -> Game
nextLevel = onLevels tail

-- | Tussenfunctie: beweeg de speler in een gegeven richting
movePlayerGame :: Dir -> Game -> Game
movePlayerGame dir = onCurrentLevel $ movePlayer dir

restoreFromBackup :: Game -> Game
restoreFromBackup g@Game{backup = (Just (p, lvls))} = g{player = p, levels = lvls}
restoreFromBackup _ = error "Er is geen backup van de game genomen."

-- --------------------------------------------------------------------
--                             Predicaten 
-- --------------------------------------------------------------------

hasNextLevel :: Game -> Bool
hasNextLevel = not . null . tail . levels

-- | Tussenfunctie: of een speler in een bepaalde richting kan bewegen
canMoveGame :: Dir -> Game -> Bool
canMoveGame dir = canMove dir . getCurrentLevel

-- | Tussenfunctie: Geeft aan of het eindpunt zich 
-- | in een bepaalde richting van de speler bevind of niet
hasEndGame :: Dir -> Game -> Bool 
hasEndGame dir = hasEndInDir dir . getCurrentLevel

-- | Tussenfunctie: of er zich in een bepaalde richting van de speler een object bevindt
-- | waarop een actie uitgevoerd kan worden.
hasActionInDirGame :: Dir -> Game -> Bool
hasActionInDirGame dir = hasActionInDir dir . getCurrentLevel

-- | Vraag alle acties op die zich in een bepaalde richting van de speler bevinden 
-- | (maar één stap in die richting)
getActionFromDirectionGame :: Dir -> Game -> ([ConditionalAction], Maybe Entity)
getActionFromDirectionGame dir g = (filterPossible actions g, mEntity)
    where (actions, mEntity) = (getActionFromDirection dir . getCurrentLevel) g

-- -------------------------------------------------
-- Functies met betrekking tot het actie paneel. Op
-- het actiepaneel bevinden zich alle - op dat moment
-- mogelijke - acties die een speler op een item of 
-- entity kan uitvoeren.
-- -------------------------------------------------

-- | Voer de actie uit die momenteel onder de selector staat
selectAction :: Game -> Game
selectAction g@Game{panelMode = PanelMode _ selPos actionList mEntity} = evalActionFunction actieFunctie mEntity g
    where actieFunctie = action (actionList !! selPos)

-- | Zet het actie paneel aan met een lijst van acties
togglePanelModeOnGame :: ([ConditionalAction], Maybe Entity) -> Game -> Game
togglePanelModeOnGame (actions, mEntity) = onPanelMode (togglePanelModeOn actions mEntity)

-- | sluit het paneel terug af
togglePanelModeOffGame :: Game -> Game
togglePanelModeOffGame = onPanelMode togglePanelModeOff 

-- | Move de selector in het actie paneel.
moveActionsSelectorGame :: Dir -> Game -> Game
moveActionsSelectorGame dir = onPanelMode (moveActionSelector dir)

-- -------------------------------------------------
--
-- -------------------------------------------------

-- | gegeven een lijst van acties, hou enkel degene over waar die conditie waar is
filterPossible :: [ConditionalAction] -> Game -> [ConditionalAction]
filterPossible actions g = filter (`evalConditionFunctions` g) actions 

-- | evalueer een enkele conditie naar waar of vals
evalConditionFunctions :: ConditionalAction -> Game -> Bool
evalConditionFunctions conAction game = all (`evalConditionFunction` game) (conditions conAction)

evalConditionFunction :: Function -> Game -> Bool
evalConditionFunction Function{fName = n, arguments = a} = evalCondition n a

-- | Alle condities die de engine ondersteund.
evalCondition :: String -> Arguments -> Game -> Bool
evalCondition "not"               (ArgFunction f) = not . evalConditionFunction f
evalCondition "inventoryFull"     (Ids [])        = inventoryFull . player
evalCondition "inventoryContains" (Ids [id])      = inventoryContains id . player
evalCondition _                _                  = error "Condition function not supported"

evalActionFunction :: Function -> Maybe Entity -> Game -> Game
evalActionFunction Function{fName = n, arguments = a} = evalAction n a

-- | Alle acties die de engine ondersteund gemapt op de implementatiefuncties
evalAction :: String -> Arguments -> Maybe Entity -> Game -> Game
evalAction "leave"            (Ids [])         _       g = leaveGame g
evalAction "useItem"          (Ids [id]) (Just entity) g = useItemGame id entity g
evalAction "increasePlayerHp" (Ids [id])       _       g = increasePlayerHpGame id g
evalAction "retrieveItem"     (Ids [id])       _       g = retrieveItemGame (getItem id g) g
evalAction "decreaseHp"       (Ids [id1, id2]) _       g = decreaseHpGame (getPlayerweapon id2 g) (getEntityGame id1 g) g
evalAction name               _                _       _ = error $ "Action function not supported: " ++ name

-- -------------------------------------------------
-- Implementaties actiefuncties van de engine.
-- -------------------------------------------------

leaveGame :: Game -> Game
leaveGame = id

useItemGame :: Id -> Entity -> Game -> Game
useItemGame itemId entity = onCurrentLevel (removeEntityFromLevel entity) 
                          . onPlayer (useItemId itemId)

increasePlayerHpGame :: Id -> Game -> Game
increasePlayerHpGame healingItemId = onPlayer (increasePlayerHp healingItemId)

retrieveItemGame :: Item -> Game -> Game
retrieveItemGame item = onCurrentLevel (removeItemFromLevel item) 
                      . onPlayer (addToInventory item)

decreaseHpGame :: Item -> Entity -> Game -> Game
decreaseHpGame weapon enemy = restartIfDead
                            . onPlayer (decreasePlayerHp enemy) 
                            . onCurrentLevel (decreaseHp weapon enemy)

-- -------------------------------------------------
--                  Hulpfuncties
-- -------------------------------------------------

restartIfDead :: Game -> Game
restartIfDead game
 | isDead (player game) = restoreFromBackup game
 | otherwise            = game

getPlayerweapon :: Id -> Game -> Item
getPlayerweapon id = searchInInventory id . player 

getItem :: Id -> Game -> Item
getItem id = searchObject id . items . getCurrentLevel

getEntityGame :: Id -> Game -> Entity
getEntityGame id = searchObject id . entities . getCurrentLevel

getAllObjectNames :: GameObject a => (Level -> [a]) -> Game -> [String]
getAllObjectNames getObjects = nub . map getName . concatMap getObjects . levels

getAllEntityNames :: Game -> [String]
getAllEntityNames = getAllObjectNames entities

getAllItemNames :: Game -> [String]
getAllItemNames = getAllObjectNames items






