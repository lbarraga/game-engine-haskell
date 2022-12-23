module RenderModule where

import TypeModule
import RenderConstants
import InputModule
import LevelChooserModule 

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Directory (listDirectory)
import Data.Map (Map, fromList, (!), member)
import Graphics.Gloss.Geometry.Angle (normalizeAngle)

type AssetMap = Map String Picture

-- | Scherm dat de gebruiker te zien krijgt wanneer hij alle levels heeft uitgespeeld.
winScreen :: Picture 
winScreen = pictures [winTextPic, winInfoTextPic]

winTextPic :: Picture
winTextPic = (translate winTextXOffset winTextYOffset . scale winTextScale winTextScale . renderText) winText

winInfoTextPic :: Picture
winInfoTextPic = (translate winHulpXOffset winHulpYOffset . scale winHelpScale winHelpScale . renderText) winHelp

-- | De initiele staat van de engine. Dit is in het levelChooser menu.
initEngineIO :: IO EngineState
initEngineIO = LevelChooser <$> initLevelSelectorIO

-- ------------------------------------------------------------------------------
-- Hier zijn een aantal functies gedefinieerd die het renderen makkelijker maken.
-- Constanten zijn te vinden in "lib/RenderConstants.hs"
-- ------------------------------------------------------------------------------

-- | Een informatieve beschrijving die gebruikt wordt in het actiepaneel.
functionDescription :: String -> Arguments -> String
functionDescription "increasePlayerHp" (Ids [id])       = "Increase hp with " ++ id
functionDescription "leave"            (Ids [])         = "Leave and do Nothing"
functionDescription "decreaseHp"       (Ids [id1, id2]) = "Decrease hp of " ++ id1 ++ " with " ++ id2
functionDescription "retrieveItem"     (Ids [id])       = "Retrieve " ++ id
functionDescription "useItem"          (Ids [id])       = "Use " ++ id
functionDescription name               _                =  name

-- Ga van een filename naar de picture van die file
png :: String -> IO Picture
png = fmap checkImage . loadJuicyPNG

checkImage :: Maybe Picture -> Picture
checkImage (Just pic) = pic
checkImage Nothing    = error "Could not load asset."

-- | Stript de extentie van een bestandsnaam.
baseName :: String -> String
baseName = takeWhile (/= '.')

-- | zet een gameCoordinaat om naar een coordinaat op het gloss venster. 
co2Gloss :: Int -> Int -> (Float, Float)
co2Gloss x y = (fromIntegral x * assetSize, fromIntegral y * assetSize)

-- | Zverplaatst een picture naar de overeenkomstige gloss coordinaat op basis van
-- | twee spelcoordinaten.
translateToGloss :: Int -> Int -> Picture -> Picture
translateToGloss x y = uncurry translate (co2Gloss x y) 

-- f [a1, a2, a3, ...] -> [a1, f a2, f (f a3), ...]
cumulateF :: (a -> a) -> [a] -> [a] 
cumulateF f [] = []
cumulateF f (x:xs) = x : cumulateF f (map f xs)

-- Verschuift de eerste picture niet, de tweede met (x, y), de derde met (2x, 2y), ...
translateCumulative :: Float -> Float -> [Picture] -> [Picture]
translateCumulative x y = cumulateF (translate x y)

-- | maakt van een lijst een lijst van bepaalde lengte 
-- | (bv bij lengte 3 wordt [1, 2] -> [Just 1, Just 2, Nothing])
extendToLength :: Int -> [a] -> [Maybe a]
extendToLength n l = map Just l ++ replicate (n - length l) Nothing

-- | Bereken de dimanties van een level.
getLevelWidth, getLevelHeight :: Level -> Float
getLevelWidth  = fromIntegral . length . head . layout
getLevelHeight = fromIntegral  . length . layout

-- | Deel een lijst van woorden op in sublijsten van een maximale grootte.
devideTextWidth :: Int -> [String] -> [[String]]
devideTextWidth _ [] = []
devideTextWidth width textWords = (take width textWords) : devideTextWidth width (drop width textWords)

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

-- | Haal de default characters van de layout uit de map
renderTile :: AssetMap -> Char -> Picture
renderTile am '.' = am ! emptyName
renderTile am '*' = am ! wallName
renderTile am 's' = pictures [am ! emptyName, am ! playerName]
renderTile am 'e' = pictures [am ! emptyName, am ! endName] 
renderTile _  _ = error "no tile"

-- | Render een Lvel bestaande uit de layout, items en entities.
renderLevel :: AssetMap -> Level -> Picture
renderLevel assetMap level = pictures [layoutPic, itemsPic, entitiesPic]
    where layoutPic   = renderLayout  assetMap (layout level)
          itemsPic    = renderObjects assetMap (items level)
          entitiesPic = renderObjects assetMap (entities level)

-- | Render een level en scale het zodat het mooi op het scherm past
renderLevelScaled :: AssetMap -> Level -> Picture
renderLevelScaled assetMap level = containWithin lvlContWidth lvlContHeight lWidth lHeight levelPic 
    where levelPic = renderLevel assetMap level 
          (lWidth, lHeight) = (getLevelWidth level, getLevelHeight level)

-- | Gegeven de breedte en hoogte van een container, samen met de breedte en hoogte van een picture, 
-- | geef de picture terug volledig binnen de breedte en hoogte van de container.
containWithin :: Float -> Float -> Float -> Float -> Picture -> Picture
containWithin containerWidth containerHeight picWidth picHeight pic = translate dx dy scaledPic
    where (dx, dy) = (calcDelta picWidth, calcDelta picHeight) 
          calcDelta side = -((assetSize / 2) * (side - 1) * scaleSize)
          scaledPic = scale scaleSize scaleSize pic
          scaleSize = min (containerHeight / (assetSize * picHeight)) (containerWidth / ( assetSize * picWidth))

-- | Render de layout van een level
renderLayout :: AssetMap -> Layout -> Picture
renderLayout assetMap = pictures . render2dWithCoords
    where render2dWithCoords l = [ translateToGloss  x y (renderTile assetMap tile)
            | (y, row)  <- zip [0..] l
            , (x, tile) <- zip [0..] row]  

-- | Render een gameObject aan de hand van de naam. Dit kunnen Items zijn maar ook Entities.
renderObjects :: GameObject a => AssetMap -> [a] -> Picture
renderObjects assetMap = pictures . map renderObject 
    where renderObject obj = translateToGloss (getX obj) (getY obj) (assetMap ! getName obj)

-- | Render alle Items in de inventory van een speler
renderInventoryItems :: AssetMap -> [Item] -> Picture
renderInventoryItems assetMap items = pictures spacedOutItemHolders 
    where spacedOutItemHolders = translateCumulative itemHolderSpace 0 itemPics
          itemPics = map (renderInventoryItem assetMap) (extendToLength 9 items)

-- | Render één enkel item in de inventory. Deze ziet er kleiner uit dan een normaal
-- | item, dus zal geresized moeten zorden om in de container te passen
renderInventoryItem :: AssetMap -> Maybe Item -> Picture
renderInventoryItem _         Nothing     = itemHolder 
renderInventoryItem assetMap (Just item)  = pictures [itemHolder, resizedItem, itemValuePic]
    where resizedItem = scale itemScale itemScale (assetMap ! getName item)
          itemScale = itemHolderSize / assetSize
          itemValuePic = (translate (-assetSize) itemValueYOffset . color white . renderText . show . itemValue) item

-- | Render de volledige inventory.
renderInventory :: AssetMap -> [Item] -> Picture
renderInventory assetMap items = pictures [inventoryBackground, inventoryItems] 
    where inventoryItems = translate (-itemHolderSpace * 4) 0 (renderInventoryItems assetMap items)

-- | Render een stuk tekst
renderText :: String -> Picture
renderText = scale textScale textScale . text

-- | Gegeven een stuk tekst, render die, maar laat maar een max breedte toe (de rest komt op de lijn eronder).
limitTextWidth :: String -> Picture
limitTextWidth = pictures . translateCumulative 0 (-spaceBetweenLimitText) . map renderText . map unwords . devideTextWidth 6 . words

-- | Render de text van een actie
renderAction :: ConditionalAction -> Picture
renderAction act = color white $ renderText (functionDescription functionName functionArgs)
    where functionName = fName (action act)
          functionArgs = arguments (action act)

-- | render alle acties in het actiepaneel
renderActions :: [ConditionalAction] -> Picture
renderActions = pictures . translateCumulative 0 (-spaceBetweenActionTexts) . map renderAction 

-- | render het actiepaneel enkel wanneer deze actief is.
renderActionPanel :: PanelMode -> Picture
renderActionPanel (PanelMode Off _ _ _ ) = blank
renderActionPanel (PanelMode On selectorPos actions mEntity) = pictures [actionPanel, actionPics, selector, info]
    where actionPics = translate actionsPicsXOffset actionsPicsYOffset $ renderActions actions
          selector = renderSelectorLine selectorPos actionSelectorWidth actionSelectorYOffset actionSelectorColor
          actionPanel = translate actionsPanelXOffset actionsPanelYOffset $ actionPanelEmpty
          info = translate actionsInfoXOffset actionsInfoYOffset $ maybe blank renderEntityInfo mEntity

-- Render info over (toepasbare) velden van een entity
renderEntityInfo :: Entity -> Picture
renderEntityInfo entity = pictures [infoContainer, entityDescriptie, entityHealth, entityStrength]
    where entityDescriptie = (color white . limitTextWidth . getDescription) entity
          entityHealth = maybe blank renderHpText (entityHp entity)
          entityStrength = maybe blank renderStrength (entityValue entity)
          infoContainer = translate infoContainerXOffset infoContainerYOffset infoContainerEmpty 

-- Render de tekst die een beschrijving geeft over de velden van een entity
renderAttributeText :: String -> Int -> Picture
renderAttributeText prefix = scale entityInfoScale entityInfoScale . color white . renderText . (prefix ++) . show

-- | Render de hp van een entity
renderHpText :: Int -> Picture
renderHpText = translate hpInfoXOffset hpInfoYOffset . renderAttributeText hpPrefix

-- | Render de strength van een entity
renderStrength :: Int -> Picture
renderStrength = translate strengthInfoXOffset strengthInfoYOffset . renderAttributeText strengthPrefix

-- | Render een selectorlijn gebruikt om dingen te selecteren
renderSelectorLine :: Int -> Float -> Float -> Color -> Picture
renderSelectorLine pos width hInset col = translate (-spaceBetweenSelectorLine) (fromIntegral (-pos + 3) * hInset - 2) selectorLine
    where selectorLine = Color col $ rectangleSolid width 1 

renderPlayerHp :: Int -> Picture
renderPlayerHp = scale playerHpScale playerHpScale . renderText . (hpPrefix ++)  . show

-- Render de volledige game bestaande uit het level, het actiepaneel, de inventaris en het hp van de speler
renderGame :: AssetMap -> Game -> Picture
renderGame getAsset g = pictures [actionPanel, inv, lvl, playerHp]
    where inv = (translate inventoryXOffset inventoryYOffset . renderInventory getAsset . inventory . player) g
          lvl = (translate lvlXOffset lvlYOffset . renderLevelScaled getAsset . head . levels) g
          actionPanel = translate actionsXOffset actionsYOffset $ renderActionPanel (panelMode g)
          playerHp = translate playerHpXOffset playerHpYOffset  $ renderPlayerHp ((hp . player) g)

-- Render het scherm waarop de gebruiker een level kan kiezen.
renderLevelChooser :: LevelSelector -> Picture
renderLevelChooser ls = scale levelChooserScale levelChooserScale $ pictures [selector, fileNames, helpText]
    where selector  = renderSelectorLine (levelSelectorPos ls) filenameSelectorWidth spaceBetweenFilenames black
          fileNames = (translate filenamesXOffset filenamesYOffset . pictures 
                    . translateCumulative 0 (-spaceBetweenFilenames) . map renderText . levelFiles) ls
          helpText  = (translate selectorHelpTextXOffset selectorHelpTextYOffset 
                    . scale helpTextScale  helpTextScale . renderText) selectorHelpText

-- Render op basis van de staat van de engine de juiste delen.
renderEngine :: AssetMap -> EngineState -> Picture
renderEngine assetMap (Playing game)     = renderGame assetMap game
renderEngine _        Won                = winScreen
renderEngine _        (LevelChooser sel) = renderLevelChooser sel

-- stap in de game
step :: Float -> EngineState -> IO EngineState
step _ = return

-- | Dit is het ingangspunt van de renderModule
-- | Hier wordt eerst een map gemaakt met de assets, 
-- | om die dan mee te geven aan alle renderfuncties die deze nodig hebben.
start :: IO ()
start = do 
    initEngine <- initEngineIO
    initLevelSelector <- initLevelSelectorIO
    
    -- Hier laden we alle assets in de asset folder. Men zou denken dat dit te veel geheugen inneemt, 
    -- maar gelukkig is haskell lazy evaluated en worden ze dus enkel ingeladen wanneer dit nodig is.
    
    fileNames <- listDirectory assetFolder
    pictures  <- mapM (png . (assetFolder ++)) fileNames
    let names = map baseName fileNames
    let assetMap = fromList (zip names pictures)

    playIO window backgroundColor fps initEngine (return . (renderEngine assetMap)) handleEngineInput step

