module RenderConstants where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy

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

assetSize :: Float
assetSize = 32 -- pxls 

-- Het Gloss venster
window :: Display
window = InWindow "RPG engine" (pxlWidth, pxlHeight) windowPosition

-- Achtergrond kleur van het gloss venster
backgroundColor :: Color
backgroundColor = makeColorI 216 181 137 255  -- 93 74 68 255

assetFolder :: String
assetFolder = "assets/fantasy/"

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

actionPanelEmptyColor :: Color
actionPanelEmptyColor = itemHolderColor

inventoryBackground :: Picture
inventoryBackground = Color inventoryBackGroundColor $ rectangleSolid inventoryWidth inventoryHeight

itemHolder :: Picture
itemHolder = Color itemHolderColor $ rectangleSolid itemHolderSize itemHolderSize

actionPanelEmpty :: Picture
actionPanelEmpty = Color actionPanelEmptyColor $ rectangleSolid 300 350

selectorLine :: Picture 
selectorLine = Color white $ rectangleSolid 250 1

lvlContWidth, lvlContHeight :: Float
lvlContWidth  = 500
lvlContHeight = 500

infoContainerWidth, infoContainerHeight :: Float
infoContainerWidth = 300
infoContainerHeight = 100

infoContainerEmpty :: Picture
infoContainerEmpty = color actionPanelEmptyColor $ rectangleSolid infoContainerWidth infoContainerHeight

emptyName, wallName, playerName, endName :: String
emptyName = "floor"
playerName = "player"
endName = "end"
wallName = "wall"
-- ----------------------------------------------------------------
--         Constanten met betrekking tot het win screen 
-- ----------------------------------------------------------------

winText, winHelp :: String
winText = "You win!"
winHelp = "Druk op SPATIE om een nieuw level te kiezen."

winTextScale, winHelpScale :: Float
winTextScale = 5
winHelpScale = 2

winTextXOffset, winTextYOffset :: Float
winTextXOffset = -110
winTextYOffset = 0

winHulpXOffset, winHulpYOffset :: Float
winHulpXOffset = -270
winHulpYOffset = -100 


-- ----------------------------------------------------------------
--       constanten met betrekking tot de volledige game 
-- ----------------------------------------------------------------

inventoryXOffset, inventoryYOffset :: Float
inventoryXOffset = 0
inventoryYOffset = -300 

lvlXOffset, lvlYOffset :: Float
lvlXOffset = 190
lvlYOffset = 50

actionsXOffset, actionsYOffset :: Float
actionsXOffset = -250
actionsYOffset = 50

playerHpXOffset, playerHpYOffset :: Float
playerHpXOffset = -400 
playerHpYOffset = 280

entityInfoScale :: Float
entityInfoScale = 1.5

hpPrefix, strengthPrefix :: String
hpPrefix = "hp: "
strengthPrefix = "Strength: "

hpInfoXOffset, hpInfoYOffset :: Float
hpInfoXOffset = 0 
hpInfoYOffset = 20

strengthInfoXOffset, strengthInfoYOffset :: Float
strengthInfoXOffset = 100
strengthInfoYOffset = hpInfoYOffset

infoContainerXOffset, infoContainerYOffset :: Float
infoContainerXOffset = 140
infoContainerYOffset = -5  

actionsPicsXOffset, actionsPicsYOffset :: Float
actionsPicsXOffset = -130
actionsPicsYOffset = 150

actionSelectorWidth, actionSelectorYOffset :: Float
actionSelectorWidth   = 250
actionSelectorYOffset = 50 

actionSelectorColor :: Color
actionSelectorColor = white

actionsPanelXOffset, actionsPanelYOffset :: Float
actionsPanelXOffset = 0
actionsPanelYOffset = 20

actionsInfoXOffset, actionsInfoYOffset :: Float
actionsInfoXOffset = -140
actionsInfoYOffset = -210

spaceBetweenActionTexts :: Float
spaceBetweenActionTexts = 50

itemValueYOffset :: Float
itemValueYOffset = 20 

spaceBetweenSelectorLine :: Float
spaceBetweenSelectorLine = 5

playerHpScale :: Float
playerHpScale = 4

-- -------------------------------------------------------------
--  constanten met betrekking tot de levelchooser
-- -------------------------------------------------------------

filenamesXOffset, filenamesYOffset :: Float
filenamesXOffset = -30
filenamesYOffset = 75

spaceBetweenFilenames :: Float
spaceBetweenFilenames = 25 

filenameSelectorWidth :: Float
filenameSelectorWidth = 50 

selectorHelpTextXOffset, selectorHelpTextYOffset :: Float
selectorHelpTextXOffset = -140
selectorHelpTextYOffset = 120

selectorHelpText :: String
selectorHelpText = "Kies een level."

helpTextScale :: Float
helpTextScale = 3

levelChooserScale :: Float
levelChooserScale = 2

spaceBetweenLimitText :: Float
spaceBetweenLimitText = 15

textScale :: Float
textScale = 0.1
