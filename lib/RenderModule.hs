module RenderModule where

import TypeModule

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ParserModule (parseGameFile)
import Text.Parsec (ParseError)
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)
import PlayerModule (decreaseHp, retrieveItem)


-- Framerate van het spel.
fps :: Int
fps = 60

--Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 80)

-- pxlWidth:  De breedte van het spel in pixels.
-- pxlHeight: De hoogte van het spel in pixels.
pxlWidth, pxlHeight :: Int
pxlWidth = 800
pxlHeight = 700

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (pxlWidth, pxlHeight) windowPosition

-- Achtergrond kleur van het gloss venster
backgroundColor :: Color
backgroundColor = white

initGame :: Game
initGame = extractGame $ parseGameFile "levels/level3.txt" 

wallPic, emptyPic :: Picture
wallPic  = png "assets/wall.png"
emptyPic = png "assets/floor.png" 

itemHolderInset, itemHolderSize, itemHolderSpace, inventoryWidth, inventoryHeight :: Float
inventoryWidth = 700
inventoryHeight = itemHolderSize + 2 * itemHolderInset
itemHolderInset = 7
itemHolderSize = 70
itemHolderSpace = itemHolderSize + itemHolderInset

inventoryBackGroundColor :: Color
inventoryBackGroundColor = makeColorI 139 69 19 255

itemHolderColor :: Color
itemHolderColor = makeColorI 72 36 10 255

inventoryBackground :: Picture
inventoryBackground = Color inventoryBackGroundColor $ rectangleSolid inventoryWidth inventoryHeight

itemHolder :: Picture
itemHolder = Color itemHolderColor $ rectangleSolid itemHolderSize itemHolderSize

actionPanelEmpty :: Picture
actionPanelEmpty = Color black $ rectangleSolid 300 400

selectorLine :: Picture 
selectorLine = Color white $ rectangleSolid 250 1

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

-- Ga van een filename naar de picture van die file
png :: String -> Picture
png = fromJust . unsafePerformIO . loadJuicyPNG

itemToPath :: Item -> String
itemToPath item = "assets/" ++ itemId item ++ ".png"

extractGame :: Either ParseError Game -> Game
extractGame (Right g) = g
extractGame _ = undefined

co2Gloss :: Int -> Int -> (Float, Float)
co2Gloss x y = (fromIntegral (x * 32), fromIntegral (y * 32))

translateToGloss :: Int -> Int -> Picture -> Picture
translateToGloss x y = uncurry translate (co2Gloss x y) 

mapWithCoordinates :: [[a]] -> (Int -> Int -> a -> b) -> [b]
mapWithCoordinates l f = [ f x y tile | (y, row) <- zip [0..] l, (x, tile) <- zip [0..] row] 

-- f [a1, a2, a3, ...] -> [a1, f a2, f (f a3), ...]
cumulateF :: (a -> a) -> [a] -> [a] 
cumulateF f [] = []
cumulateF f (x:xs) = x : cumulateF f (map f xs)

-- Verschuift de eerste picture niet, de tweede met (x, y), de derde met (2x, 2y), ...
translateCumulative :: Float -> Float -> [Picture] -> [Picture]
translateCumulative x y = cumulateF (translate x y)

extendToLength :: Int -> [a] -> [Maybe a]
extendToLength n l = map Just l ++ replicate (n - length l) Nothing

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

renderTile :: Char -> Picture
renderTile '.' = emptyPic
renderTile '*' = wallPic
renderTile 's' = emptyPic
renderTile 'e' = emptyPic
renderTile _   = error "no tile"

renderLayout :: Layout -> Picture
renderLayout layout = pictures $ mapWithCoordinates layout render 
    where render x y tile = translateToGloss x y (renderTile tile)

renderLevelItems :: [Item] -> Picture
renderLevelItems items = undefined 

renderItem :: Item -> Picture
renderItem = png . itemToPath

renderInventoryItems :: [Item] -> Picture
renderInventoryItems items = pictures spacedOutItemHolders 
    where spacedOutItemHolders = translateCumulative itemHolderSpace 0 itemPics
          itemPics = map renderInventoryItem (extendToLength 9 items)


renderInventoryItem :: Maybe Item -> Picture
renderInventoryItem Nothing     = itemHolder 
renderInventoryItem (Just item) = pictures [itemHolder, resizedItem]
    where resizedItem = scale 2.1875 2.1875 (renderItem item) -- 70 / 32

renderInventory :: [Item] -> Picture
renderInventory items = pictures [inventoryBackground, inventoryItems] 
    where inventoryItems = translate (-itemHolderSpace * 4) 0 (renderInventoryItems items)

renderText :: String -> Picture
renderText = color white . scale 0.1 0.1 . text

functionDescription :: String -> Arguments -> String
functionDescription "increasePlayerHp" (Ids [id])       = "Increase hp with " ++ id
functionDescription "leave"            (Ids [])         = "Leave and do Nothing"
functionDescription "decreaseHp"       (Ids [id1, id2]) = "Decrease hp of " ++ id1 ++ " with " ++ id2
functionDescription "retrieveItem"     (Ids [id])       = "Retrieve " ++ id
functionDescription "useItem"          (Ids [id])       = "Use " ++ id
functionDescription name               _                = error "no description for function " ++ name

renderAction :: Action -> Picture
renderAction act = renderText (functionDescription functionName functionArgs)
    where functionName = name (action act)
          functionArgs = arguments (action act)

renderActions :: [Action] -> Picture
renderActions = pictures . translateCumulative 0 50 . map renderAction 

renderActionPanel :: [Action] -> Int -> Picture
renderActionPanel actions selectorPos = pictures [actionPanelEmpty, actionPics, selector]
    where actionPics = translate (-130) 0 $ renderActions actions
          selector = translate (-5) (fromIntegral (-selectorPos + 3) * 50 - 2) selectorLine

renderGame :: Game -> Picture
--renderGame g = renderActions ((entityActions . head . entities . head . levels) g) 
renderGame g = renderActionPanel ((entityActions . head . entities . head . levels) g) 1

-- stap in de game
step :: Float -> Game -> Game
step _ game = game

handleInput :: Event -> Game -> Game
handleInput _ game = game

main :: IO ()
main = play window backgroundColor fps initGame renderGame handleInput step
