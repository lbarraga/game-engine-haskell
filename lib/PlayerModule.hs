module PlayerModule where

import TypeModule
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, isJust)

-- -------------------------------------------------------------
-- | In deze sectie staan een aantal constanten gedefinieerd   |
-- |             met betrekking tot de speler.                 |
-- -------------------------------------------------------------

-- Het maximum aantal Items dat de inventaris kan bevatten
maxInventorySize :: Int
maxInventorySize = 9

-- --------------------------------------------------------------
-- | En deze sectie zijn semantische functies gedefinieerd met
-- | betrekking tot de speler.
-- --------------------------------------------------------------

-- | Of de inventaris van een speler vol is.
inventoryFull :: Player -> Bool
inventoryFull = (== maxInventorySize) . length . inventory

-- | Of de inventaris van de speler een bepaald item bevat
inventoryContains :: Id -> Player -> Bool
inventoryContains id = isJust . safeSearchInInventory id

-- | Of de speler dood is
isDead :: Player -> Bool
isDead = (<= 0) . hp

-- | Gebruik een item, gegeven zijn id
useItemId :: Id -> Player -> Player
useItemId id player = useItem item player
    where item = searchInInventory id player

-- | Gebruik een item. Dit is gewoon zijn useTimes met 1 verminderen
-- | of verwijderen wanneer deze nog maar 1 useTime heeft
useItem :: Item -> Player -> Player
useItem item@Item{itemUseTimes = (Finite 1)} = removeFromInventory item
useItem item = onPlayerItem (onItemUseTimes (subtract 1)) item 

-- | Gebruik een healItem om de hp van de speler te verhogen
increasePlayerHp :: Id -> Player -> Player
increasePlayerHp itemId player = (useItem healItem . onPlayerHp (+ itemValue healItem)) player
    where healItem = searchInInventory itemId player

-- | Neem damage van een entity
decreasePlayerHp :: Entity -> Player -> Player
decreasePlayerHp entity = onPlayerHp (subtract entityDamage)
    where entityDamage = fromJust $ entityValue entity

-- --------------------------------------------------------------
-- Hier zijn een aantal hulpFuncties gedefinieerd.
-- --------------------------------------------------------------

-- | Zoek een gameObject op in een lijst.
-- | returns nothing wanneer het object er niet inzit.
safeSearchObject :: GameObject a => Id -> [a] -> Maybe a
safeSearchObject id = find ((==id) . getId)

-- | Zoek een GameObject op in een lijst. Error als die er niet is.
searchObject :: GameObject a => Id -> [a] -> a
searchObject id = fromJust . safeSearchObject id

-- | zoek GameObject op in de inventaris van de speker 
safeSearchInInventory :: Id -> Player -> Maybe Item
safeSearchInInventory id = safeSearchObject id . inventory

-- | Zoek een GameObject op in de inventaris. Error als die er niet is.
searchInInventory :: Id -> Player -> Item
searchInInventory id = fromJust . safeSearchInInventory id

-- | Voeg een item toe aan de inventaris van een speler.
addToInventory :: Item -> Player -> Player
addToInventory item = onPlayerInventory (++[item])

-- | Verwijder iets uit de inventaris van de speler
removeFromInventory :: Item -> Player -> Player
removeFromInventory item = onPlayerInventory (filter (/= item)) 


-- -------------------------------------------------------------
-- In deze Sectie zijn hulpFuncties gedefinieerd om code wat 
-- leesbaarder te maaken.
-- -------------------------------------------------------------

onPlayerItem :: (Item -> Item) -> Item -> Player -> Player
onPlayerItem f item = onPlayerInventory (replaceObjInList item (f item))

onPlayerItemId :: (Item -> Item) -> Id -> Player -> Player
onPlayerItemId f itemId p = onPlayerItem f (searchObject itemId (inventory p)) p

onPlayerHp :: (Int -> Int) -> Player -> Player
onPlayerHp f p = p{hp = f (hp p)}

onPlayerInventory :: ([Item] -> [Item]) -> Player -> Player
onPlayerInventory f p = p{inventory = f (inventory p)}

-- -------------------------------------------------------------
--
-- -------------------------------------------------------------














