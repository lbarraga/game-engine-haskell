module PlayerModule where

import TypeModule
import Control.Concurrent (waitQSem)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)

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
inventoryContains id = isJust . safeSearchInInventory id

-- not() is op zich ook een ondersteunde functie maar is
-- al geimplementeerd in de prelude.

-- --------------------------------------------------------------
-- | In deze sectie zijn alle acties gedefinieerd die de engine |
-- | ondersteunt. Dit zijn acties die effect hebben op de staat |
-- | van een speler.                                            |
-- --------------------------------------------------------------

debug :: a -> String -> a
debug = flip trace

leave :: Player -> Player  
leave = id

useItemId :: Id -> Player -> Player
useItemId id player = useItem item player
    where item = searchInInventory id player

useItem :: Item -> Player -> Player
useItem item@Item{itemUseTimes = (Finite 1)} = removeFromInventory item
useItem item = onPlayerItem (onItemUseTimes (subtract 1)) item 

increasePlayerHp :: Id -> Player -> Player
increasePlayerHp itemId player = (useItem healItem . onPlayerHp (+ itemValue healItem)) player
    where healItem = searchInInventory itemId player

decreasePlayerHp :: Entity -> Player -> Player
decreasePlayerHp entity = onPlayerHp (subtract entityDamage)
    where entityDamage = fromJust $ entityValue entity

-- --------------------------------------------------------------
--
-- --------------------------------------------------------------

-- --------------------------------------------------------------
--
-- --------------------------------------------------------------

safeSearchObject :: (a -> Id) -> Id -> [a] -> Maybe a
safeSearchObject getId id = find ((==id) . getId)

searchObject :: (a -> Id) -> Id -> [a] -> a
searchObject getId id = fromJust . safeSearchObject getId id

safeSearchItem :: Id -> [Item] -> Maybe Item
safeSearchItem = safeSearchObject itemId

safeSearchEntity :: Id -> [Entity] -> Maybe Entity
safeSearchEntity = safeSearchObject entityId

searchItem :: Id -> [Item] -> Item
searchItem id = fromJust . safeSearchItem id

safeSearchInInventory :: Id -> Player -> Maybe Item
safeSearchInInventory id = safeSearchItem id . inventory

searchInInventory :: Id -> Player -> Item
searchInInventory id = fromJust . safeSearchInInventory id

searchEntity :: Id -> [Entity] -> Entity
searchEntity id = fromJust . safeSearchEntity id

searchItemIndex :: Id -> [Item] -> Int
searchItemIndex id = fromJust . elemIndex id . map itemId

addToInventory :: Item -> Player -> Player
addToInventory item = onPlayerInventory (++[item])

removeFromInventory :: Item -> Player -> Player
removeFromInventory item = onPlayerInventory (filter (/= item)) 

replaceInInventory :: Item -> Item -> [Item] -> [Item]
replaceInInventory = replaceObjInList

replaceObjInList :: Eq a => a -> a -> [a] -> [a]
replaceObjInList from to l = replaceAtIndex index to l
    where index = fromJust (elemIndex from l)

-- TODO dit moet hier weg
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index repl l = before ++ [repl] ++ after
    where (before, _:after) = splitAt index l

-- -------------------------------------------------------------
--
-- -------------------------------------------------------------

onPlayerItem :: (Item -> Item) -> Item -> Player -> Player
onPlayerItem f item = onPlayerInventory (replaceInInventory item (f item))

onPlayerItemId :: (Item -> Item) -> Id -> Player -> Player
onPlayerItemId f itemId p = onPlayerItem f (searchItem itemId (inventory p)) p

onPlayerHp :: (Int -> Int) -> Player -> Player
onPlayerHp f p = p{hp = f (hp p)}

onPlayerInventory :: ([Item] -> [Item]) -> Player -> Player
onPlayerInventory f p = p{inventory = f (inventory p)}

-- -------------------------------------------------------------
--
-- -------------------------------------------------------------














