module PlayerModule where

import TypeModule
import Control.Concurrent (waitQSem)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = print "hoi"

-- -------------------------------------------------------------
-- | In deze sectie staan een aantal constanten gedefinieerd   |
-- |             met betrekking tot de speler.                 |
-- -------------------------------------------------------------

-- Het maximum aantal Items dat de inventaris kan bevatten
maxInventorySize :: Int
maxInventorySize = 9

-- -------------------------------------------------------------
-- | In deze sectie zijn de conditionele functies gedefinieerd |
-- | die de engine ondersteund. Dit zijn predicaten over       |
-- | de staat waarin de speler verkeerd.                       |
-- -------------------------------------------------------------

-- | Of de inventaris van een speler vol is.
inventoryFull :: Player -> Bool
inventoryFull = (== maxInventorySize) . length . inventory

-- | Of de inventaris van de speler een bepaald item bevat
inventoryContains :: Id -> Player -> Bool
inventoryContains id = isJust . safeSearchItem id . inventory

-- not() is op zich ook een ondersteunde functie maar is
-- al geimplementeerd in de prelude.

-- --------------------------------------------------------------
-- | In deze sectie zijn alle acties gedefinieerd die de engine |
-- | ondersteunt. Dit zijn acties die effect hebben op de staat |
-- | van een speler.                                            |
-- --------------------------------------------------------------

leave :: Player -> Player  
leave = undefined

retrieveItem :: Id -> Level -> Player -> Player
retrieveItem itemId level = addToInventory (searchItem itemId (items level))

useItem :: Id -> Player -> Player
useItem = onPlayerItem (onItemUseTimes (subtract 1))  

increasePlayerHp :: Id -> Player -> Player
increasePlayerHp itemId player = onPlayerHp (+ itemValue healItem) player
    where healItem = searchItem itemId (inventory player)

decreaseHp :: Id -> Id -> Player -> Player
decreaseHp entityId itemId = undefined 

-- --------------------------------------------------------------
--
-- --------------------------------------------------------------

evalConditionFunction :: Function -> Player -> Bool
evalConditionFunction Function{name = n, arguments = a} = evalCondition n a

evalCondition :: String -> Arguments -> Player -> Bool
evalCondition "not"               (ArgFunction f) = not . evalConditionFunction f
evalCondition "inventoryFull"     (Ids [])        = inventoryFull
evalCondition "inventoryContains" (Ids [id])      = inventoryContains id
evalCondition _                _                  = error "Condition function not supported"

evalActionFunction :: Function -> Player -> Player
evalActionFunction Function{name = n, arguments = a} = evalAction n a

evalAction :: String -> Arguments -> Player -> Player
evalAction "leave"            (Ids [])         = leave
--evalAction "retrieveItem"     (Ids [id])       = retrieveItem id
evalAction "useItem"          (Ids [id])       = useItem id
evalAction "increasePlayerHp" (Ids [id])       = increasePlayerHp id
evalAction "decreaseHp"       (Ids [id1, id2]) = decreaseHp id1 id2
evalAction _                  _                = error "Action function not supported"

-- -------------------------------------------------------------
--
-- -------------------------------------------------------------

safeSearchItem :: Id -> [Item] -> Maybe Item
safeSearchItem id = find ((==id) . itemId)

searchItem :: Id -> [Item] -> Item
searchItem id = fromJust . safeSearchItem id

searchItemIndex :: Id -> [Item] -> Int
searchItemIndex id = fromJust . elemIndex id . map itemId

addToInventory :: Item -> Player -> Player
addToInventory item = onPlayerInventory (++[item])

replaceInInventory :: Item -> Item -> [Item] -> [Item]
replaceInInventory from to l = before ++ [to] ++ after
    where (before,_:after) = splitAt index l
          index = fromJust (elemIndex from l)

onPlayerItem :: (Item -> Item) -> Id -> Player -> Player
onPlayerItem f id p = onPlayerInventory (replaceInInventory item (f item)) p
    where item = searchItem id (inventory p)















