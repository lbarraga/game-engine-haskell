module RenderModule where

import TypeModule
import LevelModule
import RenderConstants

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
import TypeModule (GameObject(getDescription), Entity (entityHp, entityValue))
import GHC.Base (undefined, Float)
import Graphics.Gloss (scale)
import RenderConstants (winText, itemHolderSize, itemHolder)

winScreen :: Picture 
winScreen = pictures [winTextPic, winInfoTextPic]

winTextPic :: Picture
winTextPic = (translate winTextXOffset winTextYOffset . scale winTextScale winTextScale . renderText) winText

winInfoTextPic :: Picture
winInfoTextPic = (translate winHulpXOffset winHulpYOffset . scale winHelpScale winHelpScale . renderText) winHelp



-- ------------------------------------------------------------------------------
--
-- Hier wijn een aantal functies gedefinieerd die het renderen makkelijker maken.
-- Constanten zijn te vinden in "lib/RenderConstants.hs"
-- ------------------------------------------------------------------------------

loadAssets :: IO (Map String Picture)
loadAssets = do
    fileNames    <- listDirectory assetFolder
    pictures <- mapM (png . ((assetFolder ++ "/") ++)) fileNames
    let names = map baseName fileNames
    return $ fromList (zip names pictures)

-- Ga van een filename naar de picture van die file
png :: String -> IO Picture
png = fmap checkImage . loadJuicyPNG

checkImage :: Maybe Picture -> Picture
checkImage (Just pic) = pic
checkImage Nothing    = error "Could not load asset."

baseName :: String -> String
baseName = takeWhile (/= '.')

objectToPic :: GameObject a => AssetMap -> a -> Picture
objectToPic assetMap = (assetMap !) . getName

co2Gloss :: Int -> Int -> (Float, Float)
co2Gloss x y = (fromIntegral x * assetSize, fromIntegral y * assetSize)

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

renderTile :: AssetMap -> Char -> Picture
renderTile am '.' = am ! emptyName
renderTile am '*' = am ! wallName
renderTile am 's' = pictures [am ! emptyName, am ! playerName]
renderTile am 'e' = pictures [am ! emptyName, am ! endName] 
renderTile _  _ = error "no tile"

renderLevel :: AssetMap -> Level -> Picture
renderLevel assetMap level = pictures [layoutPic, itemsPic, entitiesPic]
    where layoutPic   = renderLayout  assetMap (layout level)
          itemsPic    = renderObjects assetMap (items level)
          entitiesPic = renderObjects assetMap (entities level)

renderLevelScaled :: AssetMap -> Level -> Picture
renderLevelScaled assetMap level = containWithin lvlContWidth lvlContHeight lWidth lHeight levelPic 
    where levelPic = renderLevel assetMap level 
          (lWidth, lHeight) = (getLevelWidth level, getLevelHeight level)

containWithin :: Float -> Float -> Float -> Float -> Picture -> Picture
containWithin containerWidth containerHeight picWidth picHeight pic = translate dx dy scaledPic
    where (dx, dy) = (calcDelta picWidth, calcDelta picHeight) 
          calcDelta side = -((assetSize / 2) * (side - 1) * scaleSize)
          scaledPic = scale scaleSize scaleSize pic
          scaleSize = min (containerHeight / (assetSize * picHeight)) (containerWidth / ( assetSize * picWidth))

renderWithCo :: Int -> Int -> Picture -> Picture
renderWithCo x y = uncurry translate (co2Gloss x y)

renderLayout :: AssetMap -> Layout -> Picture
renderLayout assetMap = pictures . render2dWithCoords
    where render2dWithCoords l = [ renderWithCo  x y (renderTile assetMap tile)
            | (y, row)  <- zip [0..] l
            , (x, tile) <- zip [0..] row]  

renderObjects :: GameObject a => AssetMap -> [a] -> Picture
renderObjects assetMap = pictures . map renderObject 
    where renderObject obj = renderWithCo (getX obj) (getY obj) (assetMap ! getName obj)

renderInventoryItems :: AssetMap -> [Item] -> Picture
renderInventoryItems assetMap items = pictures spacedOutItemHolders 
    where spacedOutItemHolders = translateCumulative itemHolderSpace 0 itemPics
          itemPics = map (renderInventoryItem assetMap) (extendToLength 9 items)

renderInventoryItem :: AssetMap -> Maybe Item -> Picture
renderInventoryItem _         Nothing     = itemHolder 
renderInventoryItem assetMap (Just item)  = pictures [itemHolder, resizedItem, itemValuePic]
    where resizedItem = scale itemScale itemScale (assetMap ! getName item)
          itemScale = itemHolderSize / assetSize
          itemValuePic = (translate (-assetSize) itemValueYOffset . color white . renderText . show . itemValue) item

renderInventory :: AssetMap -> [Item] -> Picture
renderInventory assetMap items = pictures [inventoryBackground, inventoryItems] 
    where inventoryItems = translate (-itemHolderSpace * 4) 0 (renderInventoryItems assetMap items)

renderText :: String -> Picture
renderText = scale textScale textScale . text

limitTextWidth :: String -> Picture
limitTextWidth = pictures . translateCumulative 0 (-spaceBetweenLimitText) . map renderText . map unwords . devideTextWidth 6 . words

devideTextWidth :: Int -> [String] -> [[String]]
devideTextWidth _ [] = []
devideTextWidth width textWords = (take width textWords) : devideTextWidth width (drop width textWords)

renderAction :: ConditionalAction -> Picture
renderAction act = color white $ renderText (functionDescription functionName functionArgs)
    where functionName = fName (action act)
          functionArgs = arguments (action act)

renderActions :: [ConditionalAction] -> Picture
renderActions = pictures . translateCumulative 0 (-spaceBetweenActionTexts) . map renderAction 

renderActionPanel :: PanelMode -> Picture
renderActionPanel (PanelMode Off _ _ _ ) = blank
renderActionPanel (PanelMode On selectorPos actions mEntity) = pictures [actionPanel, actionPics, selector, info]
    where actionPics = translate actionsPicsXOffset actionsPicsYOffset $ renderActions actions
          selector = renderSelectorLine selectorPos actionSelectorWidth actionSelectorYOffset actionSelectorColor
          actionPanel = translate actionsPanelXOffset actionsPanelYOffset $ actionPanelEmpty
          info = translate actionsInfoXOffset actionsInfoYOffset $ maybe blank renderEntityInfo mEntity

renderEntityInfo :: Entity -> Picture
renderEntityInfo entity = pictures [infoContainer, entityDescriptie, entityHealth, entityStrength]
    where entityDescriptie = (color white . limitTextWidth . getDescription) entity
          entityHealth = maybe blank renderHpText (entityHp entity)
          entityStrength = maybe blank renderStrength (entityValue entity)
          infoContainer = translate infoContainerXOffset infoContainerYOffset infoContainerEmpty 

renderAttributeText :: String -> Int -> Picture
renderAttributeText prefix = scale entityInfoScale entityInfoScale . color white . renderText . (prefix ++) . show

renderHpText :: Int -> Picture
renderHpText = translate hpInfoXOffset hpInfoYOffset . renderAttributeText hpPrefix

renderStrength :: Int -> Picture
renderStrength = translate strengthInfoXOffset strengthInfoYOffset . renderAttributeText strengthPrefix

renderSelectorLine :: Int -> Float -> Float -> Color -> Picture
renderSelectorLine pos width hInset col = translate (-spaceBetweenSelectorLine) (fromIntegral (-pos + 3) * hInset - 2) selectorLine
    where selectorLine = Color col $ rectangleSolid width 1 

renderPlayerHp :: Int -> Picture
renderPlayerHp = scale playerHpScale playerHpScale . renderText . (hpPrefix ++)  . show

renderGame :: AssetMap -> Game -> Picture
renderGame getAsset g = pictures [actionPanel, inv, lvl, playerHp]
    where inv = (translate inventoryXOffset inventoryYOffset . renderInventory getAsset . inventory . player) g
          lvl = (translate lvlXOffset lvlYOffset . renderLevelScaled getAsset . head . levels) g
          actionPanel = translate actionsXOffset actionsYOffset $ renderActionPanel (panelMode g)
          playerHp = translate playerHpXOffset playerHpYOffset  $ renderPlayerHp ((hp . player) g)

renderLevelChooser :: LevelSelector -> Picture
renderLevelChooser ls = scale levelChooserScale levelChooserScale $ pictures [selector, fileNames, helpText]
    where selector  = renderSelectorLine (levelSelectorPos ls) filenameSelectorWidth spaceBetweenFilenames black
          fileNames = (translate filenamesXOffset filenamesYOffset . pictures 
                    . translateCumulative 0 (-spaceBetweenFilenames) . map renderText . levelFiles) ls
          helpText  = (translate selectorHelpTextXOffset selectorHelpTextYOffset 
                    . scale helpTextScale  helpTextScale . renderText) selectorHelpText

renderEngine :: AssetMap -> EngineState -> Picture
renderEngine assetMap (Playing game)     = renderGame assetMap game
renderEngine _        Won                = winScreen
renderEngine _        (LevelChooser sel) = renderLevelChooser sel

-- stap in de game
step :: Float -> EngineState -> IO EngineState
step _ = return

handleEngineInput :: LevelSelector -> Event -> EngineState -> IO EngineState
handleEngineInput _  ev (Playing game)     = return $ handleGameInput ev game
handleEngineInput ls ev Won                = return $ handleWinScreenInput ev ls
handleEngineInput _  ev (LevelChooser sel) = handleLevelChooserInput ev sel

handleWinScreenInput :: Event -> LevelSelector -> EngineState
handleWinScreenInput ev initLevelSelector
  | isKey KeySpace ev  = LevelChooser initLevelSelector
handleWinScreenInput _ _ = Won

handleLevelChooserInput :: Event -> LevelSelector -> IO EngineState
handleLevelChooserInput ev
  | isKey KeyUp    ev = return . LevelChooser . moveLevelSelector U
  | isKey KeyDown  ev = return . LevelChooser . moveLevelSelector D
  | isKey KeySpace ev = fmap Playing . chooseLevelFile 
handleLevelChooserInput _ = return . LevelChooser

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
  | isKey KeySpace ev = togglePanelModeOff . selectAction
handleActionPanelInput _ = id 

handleDirectionInput :: Dir -> Game -> EngineState
handleDirectionInput dir game
  | hasActionInDirGame dir game              = Playing $ togglePanelModeOn game (getActionFromDirectionGame dir game)
  | canMoveGame dir game                     = Playing $ movePlayerGame dir game
  | hasEndGame dir game && hasNextLevel game = Playing $ nextLevel game
  | hasEndGame dir game                      = Won
  | otherwise = Playing game

type AssetMap = Map String Picture

start :: IO ()
start = do 
    initEngine <- initEngineIO
    initLevelSelector <- initLevelSelectorIO
    assetMap <- loadAssets
    playIO window backgroundColor fps initEngine (return . (renderEngine assetMap)) (handleEngineInput initLevelSelector) step
















