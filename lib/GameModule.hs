module GameModule where

import TypeModule
import LevelModule (movePlayer, canMove, decreaseHp, hasActionInDir, getActionFromDirection, removeItemFromLevel, wall)
import PlayerModule (replaceAtIndex, searchInInventory, inventoryFull, inventoryContains, leave, useItem, increasePlayerHp, searchItem, addToInventory, searchEntity, decreasePlayerHp, useItemId)
import GHC.Integer (integerToInt)
import Debug.Trace (trace)
import Data.Maybe (fromJust)

onLevels :: ([Level] -> [Level]) -> Game -> Game
onLevels f g = g {levels = f (levels g)}

onPlayer :: (Player -> Player) -> Game -> Game
onPlayer f g = g{player = f (player g)}

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

hasActionInDirGame :: Dir -> Game -> Bool
hasActionInDirGame dir = hasActionInDir dir . getCurrentLevel

getActionFromDirectionGame :: Dir -> Game -> [ConditionalAction]
getActionFromDirectionGame dir g = ((`filterPossible` g) . getActionFromDirection dir . getCurrentLevel) g

-- -------------------------------------------------
--
-- -------------------------------------------------

selectAction :: Game -> Game
selectAction g@Game{panelMode = PanelMode _ selPos actionList} = evalActionFunction actieFunctie g
    where actieFunctie = action (actionList !! selPos)

togglePanelModeOn :: Game -> [ConditionalAction] -> Game
togglePanelModeOn game actions = game{panelMode = PanelMode On 0 actions} 

togglePanelModeOff :: Game -> Game
togglePanelModeOff game = game{panelMode = PanelMode Off 0 []} 

onSelectorPos :: (Int -> Int) -> Game -> Game
onSelectorPos f g@Game{panelMode = pm} = g{panelMode = pm{selectorPos = f (selectorPos pm)}}

moveSelector :: Dir -> Int -> Game -> Game
moveSelector U actionsLength = onSelectorPos ((`mod` actionsLength) . subtract 1)
moveSelector D actionsLength = onSelectorPos ((`mod` actionsLength) . (+ 1))
moveSelector _ _ = error "Cannot move selector left or right"

debug :: a -> String -> a
debug = flip trace

-- -------------------------------------------------
--
-- -------------------------------------------------

filterPossible :: [ConditionalAction] -> Game -> [ConditionalAction]
filterPossible actions g = filter (`evalConditionFunctions` g) actions 

evalConditionFunctions :: ConditionalAction -> Game -> Bool
evalConditionFunctions conAction game = all (`evalConditionFunction` game) (conditions conAction)

evalConditionFunction :: Function -> Game -> Bool
evalConditionFunction Function{fName = n, arguments = a} = evalCondition n a

evalCondition :: String -> Arguments -> Game -> Bool
evalCondition "not"               (ArgFunction f) = not . evalConditionFunction f
evalCondition "inventoryFull"     (Ids [])        = inventoryFull . player
evalCondition "inventoryContains" (Ids [id])      = inventoryContains id . player
evalCondition _                _                  = error "Condition function not supported"

evalActionFunction :: Function -> Game -> Game
evalActionFunction Function{fName = n, arguments = a} = evalAction n a

evalAction :: String -> Arguments -> Game -> Game
evalAction "leave"            (Ids [])         g = leaveGame g
evalAction "useItem"          (Ids [id])       g = useItemGame id g
evalAction "increasePlayerHp" (Ids [id])       g = increasePlayerHpGame id g
evalAction "retrieveItem"     (Ids [id])       g = retrieveItemGame (getItem id g) g
evalAction "decreaseHp"       (Ids [id1, id2]) g = decreaseHpGame (getPlayerweapon id2 g) (getEntityGame id1 g) g
evalAction name               _                _ = error $ "Action function not supported: " ++ name

functionDescription :: String -> Arguments -> String
functionDescription "increasePlayerHp" (Ids [id])       = "Increase hp with " ++ id
functionDescription "leave"            (Ids [])         = "Leave and do Nothing"
functionDescription "decreaseHp"       (Ids [id1, id2]) = "Decrease hp of " ++ id1 ++ " with " ++ id2
functionDescription "retrieveItem"     (Ids [id])       = "Retrieve " ++ id
functionDescription "useItem"          (Ids [id])       = "Use " ++ id
functionDescription name               _                = error "no description for function " ++ name

-- -------------------------------------------------
--
-- -------------------------------------------------

leaveGame :: Game -> Game
leaveGame = id

useItemGame :: Id -> Game -> Game
useItemGame itemId = onPlayer (useItemId itemId)

increasePlayerHpGame :: Id -> Game -> Game
increasePlayerHpGame healingItemId = onPlayer (increasePlayerHp healingItemId)

retrieveItemGame :: Item -> Game -> Game
retrieveItemGame item = onCurrentLevel (removeItemFromLevel item) . onPlayer (addToInventory item)

decreaseHpGame :: Item -> Entity -> Game -> Game
decreaseHpGame weapon enemy = onPlayer (decreasePlayerHp enemy) . onCurrentLevel (decreaseHp weapon enemy)

-- -------------------------------------------------
--
-- -------------------------------------------------

getPlayerweapon :: Id -> Game -> Item
getPlayerweapon id = searchInInventory id . player 

injurePlayer :: Entity -> Game -> Game
injurePlayer enemy = onPlayer (decreasePlayerHp enemy) 

getItem :: Id -> Game -> Item
getItem id = searchItem id . items . getCurrentLevel

getEntityGame :: Id -> Game -> Entity
getEntityGame id = searchEntity id . entities . getCurrentLevel







