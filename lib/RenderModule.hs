module RenderModule where

import TypeModule
import LevelModule

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ParserModule (parseGameFile)
import Text.Parsec (ParseError)
import Data.Maybe (fromJust)
import System.Directory (listDirectory)
import GameModule
import Debug.Trace (trace)
import EngineModule (EngineState (Playing, Won, LevelChooser), chooseLevelFile)
import LevelChooserModule (LevelSelector (levelFiles, levelSelectorPos, LevelSelector), initLevelSelectorIO, moveLevelSelector)
import Data.Map (Map, fromList, (!), member)

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
window = InWindow "RPG engine" (pxlWidth, pxlHeight) windowPosition

-- Achtergrond kleur van het gloss venster
backgroundColor :: Color
backgroundColor = makeColorI 216 181 137 255  -- 93 74 68 255

initEngineIO :: IO EngineState
initEngineIO = LevelChooser <$> initLevelSelectorIO

assetFolder :: String
assetFolder = "assets/fantasy"

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

winScreen :: Picture 
winScreen = pictures [win, info]
    where win = (translate (-110) 0 . scale 5 5 . renderText) "You win!"
          info = (translate (-270) (-100) . scale 2 2 . renderText) "Druk op SPATIE om een nieuw level te kiezen."

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

-- Ga van een filename naar de picture van die file

loadAssets :: IO (Map String Picture)
loadAssets = do
    fileNames    <- listDirectory assetFolder
    pictures <- mapM (png . ((assetFolder ++ "/") ++)) fileNames
    let names = map baseName fileNames
    return $ fromList (zip names pictures)

png :: String -> IO Picture
png = fmap checkImage . loadJuicyPNG

checkImage :: Maybe Picture -> Picture
checkImage (Just pic) = pic
checkImage Nothing    = error "Could not load asset."

baseName :: String -> String
baseName = takeWhile (/= '.')

--itemToPath :: Item -> String
--itemToPath item = assetFolder ++ "/" ++ itemName item ++ ".png"

--entityToPath :: Entity -> String
--entityToPath entity = assetFolder ++ "/" ++ entityName entity ++ ".png"

objectToPic :: GameObject a => AssetMap -> a -> Picture
objectToPic assetMap = (assetMap !) . getName

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

(!!!) :: AssetMap -> String -> Picture
(!!!) assetMap key
  | key `member` assetMap = assetMap ! key
  | otherwise = error $ "Key '" ++ key ++ "' is niet gevonden!"

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

renderTile :: AssetMap -> Char -> Picture
renderTile am '.' = am !!! "floor"
renderTile am '*' = am !!! "wall"
renderTile am 's' = pictures [am !!! "floor", am !!! "player"]
renderTile am 'e' = pictures [am !!! "floor", am !!! "end"] 
renderTile _  _ = error "no tile"

renderLevel :: AssetMap -> Level -> Picture
renderLevel assetMap level = pictures [translate levelDx levelDy scaledLevel , levelContainerEmpty]
    where (levelDx, levelDy) = (-(16 * (lWidth - 1) * scaleSize), -(16 * (lHeight - 1) * scaleSize))
          scaledLevel = scale scaleSize scaleSize levelPic  
          levelPic = pictures [renderLayout assetMap (layout level), renderObjects assetMap (items level), renderObjects assetMap (entities level)]
          scaleSize = min (lvlContHeight / (32 * lHeight)) (lvlContWidth / ( 32 * lWidth))
          (lWidth, lHeight) = (getLevelWidth level, getLevelHeight level)

renderWithCo :: Int -> Int -> Picture -> Picture
renderWithCo x y = translate dx dy
    where dx = 32 * fromIntegral x
          dy = 32 * fromIntegral y

renderLayout :: AssetMap -> Layout -> Picture
renderLayout assetMap = pictures . render2dWithCoords
    where render2dWithCoords l = [ renderWithCo  x y (renderTile assetMap tile)
            | (y, row)  <- zip [0..] l
            , (x, tile) <- zip [0..] row]  

renderObjects :: GameObject a => AssetMap -> [a] -> Picture
renderObjects assetMap = pictures . map renderObject 
    where renderObject obj = renderWithCo (getX obj) (getY obj) (assetMap !!! getName obj)

--renderItem :: Item -> Picture
--renderItem = png . itemToPath

-- renderEntities :: GameObject a => (a -> Picture) -> [Entity] -> Picture
-- renderEntities getAsset = pictures . map (renderWithCo getAsset)

renderEntity :: AssetMap -> Entity -> Picture
renderEntity assetMap entity = pictures [entityPic, entityHpPic]
    where entityPic = assetMap !!! getName entity
          entityHpPic = (translate 0 20 . maybe blank renderHp . entityHp) entity

renderHp ::  Int -> Picture
renderHp eHp = pictures [hpContainer, hpText]
    where hpContainer = color black $ rectangleSolid 10 10
          hpText      = (translate (-4) (-2) . scale 0.5 0.5 . color white . renderText . show) eHp

renderInventoryItems :: AssetMap -> [Item] -> Picture
renderInventoryItems assetMap items = pictures spacedOutItemHolders 
    where spacedOutItemHolders = translateCumulative itemHolderSpace 0 itemPics
          itemPics = map (renderInventoryItem assetMap) (extendToLength 9 items)

renderInventoryItem :: AssetMap -> Maybe Item -> Picture
renderInventoryItem _         Nothing     = itemHolder 
renderInventoryItem assetMap (Just item)  = pictures [itemHolder, resizedItem, itemValuePic]
    where resizedItem = scale 2.1875 2.1875 (assetMap !!! getName item) -- 70 / 32
          itemValuePic = (translate (-32) 20 . color white . renderText . show . itemValue) item

renderInventory :: AssetMap -> [Item] -> Picture
renderInventory assetMap items = pictures [inventoryBackground, inventoryItems] 
    where inventoryItems = translate (-itemHolderSpace * 4) 0 (renderInventoryItems assetMap items)

renderText :: String -> Picture
renderText = scale 0.1 0.1 . text


renderAction :: ConditionalAction -> Picture
renderAction act = color white $ renderText (functionDescription functionName functionArgs)
    where functionName = fName (action act)
          functionArgs = arguments (action act)

renderActions :: [ConditionalAction] -> Picture
renderActions = pictures . translateCumulative 0 (-50) . map renderAction 

renderActionPanel :: PanelMode -> Picture
renderActionPanel (PanelMode Off _ _ _ ) = blank
renderActionPanel (PanelMode On selectorPos actions _) = pictures [actionPanelEmpty, actionPics, selector]
    where actionPics = translate (-130) 150 $ renderActions actions
          selector = renderSelectorLine selectorPos 250 50 white 

renderSelectorLine :: Int -> Float -> Float -> Color -> Picture
renderSelectorLine pos width hInset col = translate (-5) (fromIntegral (-pos + 3) * hInset - 2) selectorLine
    where selectorLine = Color col $ rectangleSolid width 1 

renderPlayerHp :: Int -> Picture
renderPlayerHp = scale 4 4 . renderText . ("hp: " ++)  . show

renderGame :: AssetMap -> Game -> Picture
renderGame getAsset g = pictures [actionPanel, inv, lvl, playerHp]
    where inv = (translate 0 (-300) . renderInventory getAsset . inventory . player) g
          lvl = (translate 190 50 . renderLevel getAsset . head . levels) g
          actionPanel = translate (-250) 50 $ renderActionPanel (panelMode g)
          playerHp = translate (-400) 280 $ renderPlayerHp ((hp . player) g)

renderLevelChooser :: LevelSelector -> Picture
renderLevelChooser ls = scale 2 2 $ pictures [selector, fileNames, helpText]
    where selector = renderSelectorLine (levelSelectorPos ls) 50 25 black
          fileNames = (translate (-30) 75 . pictures . translateCumulative 0 (-25) . map renderText . levelFiles) ls
          helpText = (translate (-140) 120 . scale 3 3 . renderText) "Kies een level."

renderEngine :: AssetMap -> EngineState -> Picture
renderEngine assetMap (Playing game)     = renderGame assetMap game
renderEngine _        Won                = winScreen
renderEngine _        (LevelChooser sel) = renderLevelChooser sel

-- stap in de game
step :: Float -> EngineState -> EngineState
step _ engine = engine

handleEngineInput :: LevelSelector -> Event -> EngineState ->  EngineState
handleEngineInput _  ev (Playing game)     = handleGameInput ev game
handleEngineInput ls ev Won                = handleWinScreenInput ev ls
handleEngineInput _  ev (LevelChooser sel) = handleLevelChooserInput ev sel

handleWinScreenInput :: Event -> LevelSelector -> EngineState
handleWinScreenInput ev initLevelSelector
  | isKey KeySpace ev  = LevelChooser initLevelSelector
handleWinScreenInput _ _ = Won

handleLevelChooserInput :: Event -> LevelSelector -> EngineState
handleLevelChooserInput ev
  | isKey KeyUp    ev = LevelChooser . moveLevelSelector U
  | isKey KeyDown  ev = LevelChooser . moveLevelSelector D
  | isKey KeySpace ev = Playing      . chooseLevelFile 
handleLevelChooserInput _ = LevelChooser

handleGameInput :: Event -> Game -> EngineState
handleGameInput ev game 
  | panelStatus == On  = Playing $ handleActionPanelInput ev game
  | otherwise          = handlePlayerInput ev game
  where panelStatus = (status . panelMode) game

handlePlayerInput :: Event -> Game -> EngineState
handlePlayerInput ev
  | isKey KeyDown  ev = handleDirectionInput D
  | isKey KeyUp    ev = handleDirectionInput U
  | isKey KeyRight ev = handleDirectionInput R
  | isKey KeyLeft  ev = handleDirectionInput L
handlePlayerInput _   = Playing

handleActionPanelInput :: Event -> Game -> Game
handleActionPanelInput ev 
  | isKey KeyDown  ev = moveSelector D 
  | isKey KeyUp    ev = moveSelector U 
  | isKey KeySpace ev = (togglePanelModeOff . selectAction)
handleActionPanelInput _ = id 

handleDirectionInput :: Dir -> Game -> EngineState
handleDirectionInput dir game
  | hasActionInDirGame dir game              = Playing $ togglePanelModeOn game (getActionFromDirectionGame dir game)
  | canMoveGame dir game                     = Playing $ movePlayerGame dir game
  | hasEndGame dir game && hasNextLevel game = Playing $ nextLevel game
  | hasEndGame dir game                      = Won
  | otherwise = Playing game

type AssetMap = Map String Picture

main :: IO ()
main = do 
    initEngine <- initEngineIO
    initLevelSelector <- initLevelSelectorIO
    assetMap <- loadAssets
    play window backgroundColor fps initEngine (renderEngine assetMap) (handleEngineInput initLevelSelector) step
















