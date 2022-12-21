module GameModule where

import TypeModule
import LevelModule (movePlayer, canMove, decreaseHp, hasActionInDir, getActionFromDirection, removeItemFromLevel, wall, removeEntityFromLevel, hasEndInDir)
import PlayerModule (replaceAtIndex, searchInInventory, inventoryFull, inventoryContains, leave, useItem, increasePlayerHp, searchItem, addToInventory, searchEntity, decreasePlayerHp, useItemId, isDead)
import GHC.Integer (integerToInt)
import Debug.Trace (trace)
import Data.Maybe (fromJust)
import ParserModule (parseGameFile)


onLevels :: ([Level] -> [Level]) -> Game -> Game
onLevels f g = g {levels = f (levels g)}

onPlayer :: (Player -> Player) -> Game -> Game
onPlayer f g = g{player = f (player g)}

onCurrentLevel :: (Level -> Level) -> Game -> Game
onCurrentLevel f = onLevels (\levels -> replaceAtIndex 0 (f (head levels)) levels)

 -- ----------------------------------------------------
 --  Een aantal semantische functies in een game
 -- ----------------------------------------------------

getCurrentLevel :: Game -> Level
getCurrentLevel = head . levels

-- | Ga naar het volgende Level
nextLevel :: Game -> Game
nextLevel = onLevels tail

hasNextLevel :: Game -> Bool
hasNextLevel = not . null . tail . levels
-- | Tussenfunctie: beweeg de speler in een gegeven richting
movePlayerGame :: Dir -> Game -> Game
movePlayerGame dir = onCurrentLevel $ movePlayer dir

-- | Tussenfunctie: of een speler in een bepaalde richting kan bewegen
canMoveGame :: Dir -> Game -> Bool
canMoveGame dir = canMove dir . getCurrentLevel

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
togglePanelModeOn :: Game -> ([ConditionalAction], Maybe Entity) -> Game
togglePanelModeOn game (actions, mEntity) = game{panelMode = PanelMode On 0 actions mEntity} 

-- | sluit het paneel terug af
togglePanelModeOff :: Game -> Game
togglePanelModeOff game = game{panelMode = PanelMode Off 0 [] Nothing} 

-- | Pas een functie toe op de selector positie van het actie paneel 
onSelectorPos :: (Int -> Int) -> Game -> Game
onSelectorPos f g@Game{panelMode = pm} = g{panelMode = pm{selectorPos = f (selectorPos pm)}}

-- | Move de selector in het actie paneel.
moveSelector :: Dir -> Int -> Game -> Game
moveSelector U actionsLength = onSelectorPos ((`mod` actionsLength) . subtract 1)
moveSelector D actionsLength = onSelectorPos ((`mod` actionsLength) . (+ 1))
moveSelector _ _ = error "Cannot move selector left or right"

debug :: a -> String -> a
debug = flip trace

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

-- | Een informatieve beschrijving die gebruikt wordt in het actiepaneel.
functionDescription :: String -> Arguments -> String
functionDescription "increasePlayerHp" (Ids [id])       = "Increase hp with " ++ id
functionDescription "leave"            (Ids [])         = "Leave and do Nothing"
functionDescription "decreaseHp"       (Ids [id1, id2]) = "Decrease hp of " ++ id1 ++ " with " ++ id2
functionDescription "retrieveItem"     (Ids [id])       = "Retrieve " ++ id
functionDescription "useItem"          (Ids [id])       = "Use " ++ id
functionDescription name               _                = error "no description for function " ++ name

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
  | isDead (player game) = parseGameFile "levels/level4.txt"
  | otherwise            = game

getPlayerweapon :: Id -> Game -> Item
getPlayerweapon id = searchInInventory id . player 

getItem :: Id -> Game -> Item
getItem id = searchItem id . items . getCurrentLevel

getEntityGame :: Id -> Game -> Entity
getEntityGame id = searchEntity id . entities . getCurrentLevel







