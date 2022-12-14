module RenderModule where

import TypeModule

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ParserModule (parseGameFile)
import Text.Parsec (ParseError)
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)


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
backgroundColor = makeColorI 221 160 221 1

initGame :: Game
initGame = extractGame $ parseGameFile "levels/level2.txt" 

wallPic, emptyPic :: Picture
wallPic  = png "assets/wall.png"
emptyPic = png "assets/floor.png" 

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

-- Ga van een filename naar de picture van die file
png :: String -> Picture
png = fromJust . unsafePerformIO . loadJuicyPNG

extractGame :: Either ParseError Game -> Game
extractGame (Right g) = g
extractGame _ = undefined

co2Gloss :: Int -> Int -> (Float, Float)
co2Gloss x y = (fromIntegral (x * 32), fromIntegral (y * 32))

-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

renderTile :: Char -> Int -> Int -> Picture
renderTile '.' x y = emptyPic
renderTile '*' x y = wallPic
renderTile 's' x y = wallPic
renderTile 'e' x y = wallPic
renderTile _   _ _ = error "no tile"

renderLayout :: Layout -> Picture
renderLayout layout = pictures $ map pictures pictures2d  
    where pictures2d = [[uncurry translate (co2Gloss x y) (renderTile (layout !! y !! x) x y) | x <- [0..(width - 1)]] | y <- [0..(height - 1)]]
          height = length layout 
          width = length $ head layout 

renderGame :: Game -> Picture
renderGame g = (scale 3 3 . renderLayout . layout . head .levels) initGame

-- stap in de game
step :: Float -> Game -> Game
step _ game = game

handleInput :: Event -> Game -> Game
handleInput _ game = game

main :: IO ()
main = play window backgroundColor fps initGame renderGame handleInput step
