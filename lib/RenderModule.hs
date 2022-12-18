module RenderModule where

import TypeModule
import LevelModule

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ParserModule (parseGameFile)
import Text.Parsec (ParseError)
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)
import PlayerModule (decreaseHp, retrieveItem, leave)
import GameModule (movePlayerGame, canMoveGame)


-- Framerate van het spel.
fps :: Int
fps = 60

--Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 80)

-- pxlWidth:  De breedte van het spel in pixels.
-- pxlHeight: De hoogte van het spel in pixels.
pxlWidth, pxlHeight :: Int
pxlWidth = 900
pxlHeight = 750

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (pxlWidth, pxlHeight) windowPosition

-- Achtergrond kleur van het gloss venster
backgroundColor :: Color
backgroundColor = makeColorI 216 181 137 255  -- 93 74 68 255

initGame :: Game
initGame = extractGame $ parseGameFile "levels/level3.txt" 

wallPic, emptyPic, playerPic, endPic :: Picture
wallPic   = png "assets/wall.png"
emptyPic  = png "assets/floor.png" 
playerPic = png "assets/player.png"
endPic    = png "assets/end.png"

itemHolderInset, itemHolderSize, itemHolderSpace, inventoryWidth, inventoryHeight :: Float
inventoryWidth = 700
inventoryHeight = itemHolderSize + 2 * itemHolderInset
itemHolderInset = 7
itemHolderSize = 70
itemHolderSpace = itemHolderSize + itemHolderInset

inventoryBackGroundColor :: Color
inventoryBackGroundColor = makeColorI 139 69 19 255

levelContainerColor :: Color
levelContainerColor = red

itemHolderColor :: Color
itemHolderColor = makeColorI 72 36 10 255

actionPanelEmptyColor :: Color
actionPanelEmptyColor = itemHolderColor

inventoryBackground :: Picture
inventoryBackground = Color inventoryBackGroundColor $ rectangleSolid inventoryWidth inventoryHeight

itemHolder :: Picture
itemHolder = Color itemHolderColor $ rectangleSolid itemHolderSize itemHolderSize

actionPanelEmpty :: Picture
actionPanelEmpty = Color actionPanelEmptyColor $ rectangleSolid 300 400

selectorLine :: Picture 
selectorLine = Color white $ rectangleSolid 250 1

lvlContWidth, lvlContHeight :: Float
lvlContWidth  = 500
lvlContHeight = 500

levelContainerEmpty :: Picture
levelContainerEmpty = Color levelContainerColor $ rectangleWire lvlContWidth lvlContHeight

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

-- Ga van een filename naar de picture van die file
png :: String -> Picture
png = checkImage . unsafePerformIO . loadJuicyPNG

checkImage :: Maybe Picture -> Picture
checkImage (Just pic) = pic
checkImage Nothing    = error "Could not load asset."

itemToPath :: Item -> String
itemToPath item = "assets/" ++ itemId item ++ ".png"

entityToPath :: Entity -> String
entityToPath entity = "assets/" ++ entityId entity ++ ".png"

extractGame :: Either ParseError Game -> Game
extractGame (Right g) = g
extractGame (Left err) = error (show err)

co2Gloss :: Int -> Int -> (Float, Float)
co2Gloss x y = (fromIntegral (x * 32), fromIntegral (y * 32))

translateToGloss :: Int -> Int -> Picture -> Picture
translateToGloss x y = uncurry translate (co2Gloss x y) 

-- f [a1, a2, a3, ...] -> [a1, f a2, f (f a3), ...]
cumulateF :: (a -> a) -> [a] -> [a] 
cumulateF f [] = []
cumulateF f (x:xs) = x : cumulateF f (map f xs)

-- Verschuift de eerste picture niet, de tweede met (x, y), de derde met (2x, 2y), ...
translateCumulative :: Float -> Float -> [Picture] -> [Picture]
translateCumulative x y = cumulateF (translate x y)

extendToLength :: Int -> [a] -> [Maybe a]
extendToLength n l = map Just l ++ replicate (n - length l) Nothing

getLevelWidth, getLevelHeight :: Level -> Float
getLevelWidth  = fromIntegral . length . head . layout
getLevelHeight = fromIntegral  . length . layout

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

renderTile :: Char -> Picture
renderTile '.' = emptyPic
renderTile '*' = wallPic
renderTile 's' = pictures [emptyPic, playerPic]
renderTile 'e' = pictures [emptyPic, endPic] 
renderTile _   = error "no tile"

renderLevel :: Level -> Picture
renderLevel level = pictures [translate levelDx levelDy scaledLevel , levelContainerEmpty]
    where (levelDx, levelDy) = (-(16 * (lWidth - 1) * scaleSize), -(16 * (lHeight - 1) * scaleSize))
          scaledLevel = scale scaleSize scaleSize levelPic  
          levelPic = pictures [renderLayout (layout level), renderItems (items level), renderEntities (entities level)]
          scaleSize = min (lvlContHeight / (32 * lHeight)) (lvlContWidth / ( 32 * lWidth))
          (lWidth, lHeight) = (getLevelWidth level, getLevelHeight level)

renderWithCo :: (a -> Int) -> (a -> Int) -> (a -> Picture) -> a -> Picture
renderWithCo getX getY render obj = translate dx dy $ render obj
    where dx = 32 * fromIntegral (getX obj)
          dy = 32 * fromIntegral (getY obj)

renderLayout :: Layout -> Picture
renderLayout = pictures . render2dWithCoords
    where render2dWithCoords l = [ renderWithCo (const x) (const y) renderTile tile 
            | (y, row)  <- zip [0..] l
            , (x, tile) <- zip [0..] row]  

renderItems :: [Item] -> Picture
renderItems = pictures . map (renderWithCo itemX itemY renderItem)

renderItem :: Item -> Picture
renderItem = png . itemToPath

renderEntities :: [Entity] -> Picture
renderEntities = pictures . map (renderWithCo entityX entityY renderEntity)

renderEntity :: Entity -> Picture
renderEntity = png . entityToPath

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

-- TODO deze functie moet hier weg
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
renderGame g = pictures [actionPanel, inv, lvl]
    where inv = (translate 0 (-300) . renderInventory . inventory . player) g
          lvl = (translate 190 50 . renderLevel . head . levels) g
          actionPanel = translate (-250) 50 actionPanelEmpty

-- stap in de game
step :: Float -> Game -> Game
step _ game = game

handleInput :: Event -> Game -> Game
handleInput ev game@Game{levels = lvls}
  | isKey KeyDown  ev && canMoveGame D game = movePlayerGame D game
  | isKey KeyUp    ev && canMoveGame U game = movePlayerGame U game
  | isKey KeyRight ev && canMoveGame R game = movePlayerGame R game
  | isKey KeyLeft  ev && canMoveGame L game = movePlayerGame L game
handleInput _ game = game 

main :: IO ()
main = play window backgroundColor fps initGame renderGame handleInput step
