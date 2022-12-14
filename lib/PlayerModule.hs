module PlayerModule where

import TypeModule
import Control.Concurrent (waitQSem)

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
inventoryContains :: String -> Player -> Bool
inventoryContains id = elem id . map itemId . inventory

-- not() is op zich ook een ondersteunde functie maar is
-- al geimplementeerd in de prelude.

-- --------------------------------------------------------------
-- | In deze sectie zijn alle acties gedefinieerd die de engine |
-- | ondersteunt. Dit zijn acties die effect hebben op de staat |
-- | van een speler.                                            |
-- --------------------------------------------------------------

leave :: Player -> Player  
leave = undefined

retrieveItem, useItem, increasePlayerHp :: String -> Player -> Player
retrieveItem     itemId = undefined 
useItem          itemId = undefined 
increasePlayerHp itemId = undefined 

decreaseHp :: String -> String -> Player -> Player
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
evalAction "retrieveItem"     (Ids [id])       = retrieveItem id
evalAction "useItem"          (Ids [id])       = useItem id
evalAction "increasePlayerHp" (Ids [id])       = increasePlayerHp id
evalAction "decreaseHp"       (Ids [id1, id2]) = decreaseHp id1 id2
evalAction _                  _                = error "Action function not supported"

-- -------------------------------------------------------------
--
-- -------------------------------------------------------------

-- | Zoek een item in een level gegeven het id van dat item.
searchItem :: Id -> Level -> Item
searchItem id = head . filter ((== id) . itemId) . items

addToInventory :: Item -> Player -> Player
addToInventory item p@Player{inventory = inv} = p{inventory = inv ++ [item]}

replaceInInventory :: Item -> Player -> Player
replaceInInventory item player = undefined

decreaseUseTime :: UseTime Int -> UseTime Int
decreaseUseTime Infinite   = Infinite
decreaseUseTime (Finite n) = Finite (n - 1)





















