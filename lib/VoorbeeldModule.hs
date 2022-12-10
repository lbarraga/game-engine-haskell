module VoorbeeldModule where

import Numeric (readFloat, readHex, readSigned)
import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)

parse :: String -> IO Game
parse fileName = parseFromFile pGame fileName >>= either report return
  where report err = exitFailure

main :: IO ()
main = do
     result <- parseFromFile pGame "levels/level2.txt"
     case result of
       Left err     -> print err
       Right output -> print output

-- | een veld van een object of van een entitie. 
-- | omdat objecten en entities heel veel gemeenschappelijke
-- | velden hebben, is er geen onderscheid gemaakt.
data ObjectField = Id String 
                | X Int
                | Y Int
                | Hp Int
                | Name String
                | Description String     
                | UseTimes UseTime
                | Direction Dir
                | ObjectValue Int
                | Inventory [Object]
                | Actions [Action]
                deriving (Eq, Show)

-- | De richting van een object
data Dir = U | D | L | R deriving (Eq, Show)
data UseTime = Finite Int | Infinite deriving (Eq, Show)

-- | een identifier van een object
type Id = String

-- | Het argument van een functie is ofwel nog een functie (bv. bij not(...)),
-- | ofwel één of meerdere ids
data Arguments = Function Function | Ids [Id] deriving (Eq, Show)

-- | Een functie heeft een naam en argumenten
data Function = Funtion {
    name :: String,
    arguments :: Arguments
} deriving (Eq, Show)

-- | een object of entity
type Object = [ObjectField]

-- | een actie is heeft bepaalde condities en een actie 
-- | om uit te voeren wanneer deze condities allemaal waar zijn
data Action = Action {
    conditions :: [Function],
    action :: Function
} deriving (Eq, Show)

-- | Parser voor ObjectField
pObjectField :: CharParser () ObjectField
pObjectField = value <* spaces
    where value = Id          <$> pField pString    "id"
              <|> X           <$> pField pNumber    "x"
              <|> Y           <$> pField pNumber    "y"
              <|> Hp          <$> pField pNumber    "hp"
              <|> Name        <$> pField pString    "name"
              <|> Description <$> pField pString    "description"
              <|> Direction   <$> pField pDirection "direction"
              <|> UseTimes    <$> pField pUseTime   "useTimes"
              <|> ObjectValue <$> pField pNumber    "value"
              <|> Actions     <$> pField pActions   "actions"
              <|> Inventory   <$> pField pInventory "inventory"
              <?> "ObjectField"

-- | Een speler heeft hp en een inventaris.
data Player = Player{hp :: Int, attributes :: [Object]} deriving (Eq, Show)

-- | De level-layout
type Layout = [[Char]]

-- | Een level bestaat uit een layout, 
-- | een lijst van objecten en een lijst van entities.
data Level = Level {
    layout :: Layout, 
    items :: [Object], 
    entities :: [Object]
} deriving (Eq, Show) 

-- | Een Game heeft een speler en een lijst van levels.
data Game = Game{player :: Player, levels :: [Level]} deriving (Eq, Show)

-- | Parser van de volledige game. Ook het ingangspunt van alle parsers.
-- | Deze parser zal worden opgeroepen op het bestand.
pGame :: CharParser () Game
pGame = Game <$> (spaces *> pPlayer) <*> (spaces *> pLevels)

-- | Parsen van meerdere levels
pLevels :: CharParser () [Level]
pLevels = pField (pListOf (pBetween '{' '}' pLevel)) "levels"

-- | Parsen van een enkel level (zonder '{' en '}')
pLevel :: CharParser () Level
pLevel = Level <$> (spaces *> pLayoutField <* char ',') 
               <*> (spaces *> pItems <* char ',') 
               <*> (spaces *> pEntities <* spaces)

-- | Parsen van het items-veld.
pItems :: CharParser () [Object]
pItems = pField pObjects "items"

-- | Parsen van het entities-veld.
pEntities :: CharParser () [Object]
pEntities = pField pObjects "entities"

-- | De mogelijke karakters die een tile in de gameLayout voorstellen.
pWall, pEmpty, pStart, pEnd :: CharParser () Char
pWall  = char '*'
pEmpty = char '.'
pStart = char 's'
pEnd   = char 'e'

-- | één enkele tile in de gamelayout
pTile :: CharParser () Char
pTile = pWall <|> pEmpty <|> pStart <|> pEnd

-- | Een rij van Tiles parsen
pTileRow :: CharParser () [Char]
pTileRow = pTile `sepBy` char ' '

-- | Parser voor het layout-veld.
pLayoutField :: CharParser () Layout
pLayoutField = spaces *> pField (pBetween '{' '}' pLayout) "layout"

-- Parser voor de layout. de layout wordt gezien als een lijst met als
-- delimiter 'whitespace...| '
pLayout :: CharParser () Layout
pLayout = delim *> (pTileRow `sepBy` try delim) <* fDelim
    where delim = fDelim *> string "| "
          fDelim = char '\n' *> many (char ' ')

-- | Parsen van het speler-veld.
pPlayer :: CharParser () Player
pPlayer = pField (pBetween '{' '}' pPlayerInner) "player"

-- | Parsen van de speler (zonder '{' en '}')
pPlayerInner :: CharParser () Player
pPlayerInner = Player <$> hp <*> inventory
    where hp        = spaces *> pField pNumber "hp" <* char ','
          inventory = spaces *> pField pInventory "inventory" <* spaces

-- | Parsen van een lijst met geen of meerdere objecten
pObjects :: CharParser () [Object]
pObjects = pListOf pObject 

-- | parsen van een object/entitie
pObject :: CharParser () Object
pObject = pObjectOf pObjectField

-- | inventaries van een speler parsen
pInventory :: CharParser () [Object]
pInventory = pObjects

-- | Meerdere acties parsen
pActions :: CharParser  () [Action]
pActions = pObjectOf pAction

-- | Een enkele functie parsen.
-- | Eerst wordten de conditionele functies geparsed en daarna de actie
pAction :: CharParser () Action
pAction = Action <$> pListOf pFunction <*> (spaces *> pFunction)

-- | Parsen van een functie 
pFunction :: CharParser () Function
pFunction = Funtion <$> pFunctionName <*> pFunctionArguments

pFunctionName :: CharParser () String
pFunctionName = pCharsButNot ",(]" <* char '(' 

-- | Parsen van functie-argumenten. ofwel opnieuw een functie ofwel ids.
pFunctionArguments :: CharParser () Arguments
pFunctionArguments = (Ids <$> try pIds) <|> (Function <$> pFunction <* char ')') 

-- Object- of entitie-id
pIds :: CharParser () [String]
pIds = (pSingleId `sepBy` char ',') <* char ')'

pSingleId :: CharParser () String
pSingleId  = spaces *> pCharsButNot ", ()" <* spaces

-- | meerdere karakters die niet in een gegeven lijst zitten
pCharsButNot :: [Char] -> CharParser () String
pCharsButNot excluded = many1 $ satisfy (`notElem` excluded)

-- Parse een veld met een sleutelWaarde met een gegeven parser
pField :: CharParser () a -> String -> CharParser () a
pField valueParser key = try $ (string key >> char ':' >> spaces) >> valueParser

-- | Parse een string in doublequotes
pString :: CharParser () String
pString = pBetween '\"' '\"' (pCharsButNot "\"")

-- | Parse de richting van een object.
pDirection :: CharParser () Dir
pDirection = U <$ string "up" 
         <|> D <$ string "down" 
         <|> R <$ string "right" 
         <|> L <$ string "left"

-- | Parse de useTime van een object. Dit kan een eindig aantal keren zijn,
-- | maar ook oneindig
pUseTime :: CharParser () UseTime
pUseTime = (Infinite <$ string "infinite") <|> (Finite <$> pNumber)

readInt :: String -> Int
readInt = read

-- | Parse een getal naar een Int
pNumber :: CharParser () Int
pNumber = readInt <$> many1 digit

-- | Parse tussen twee chars. 'between' bestaat al maar dit is properder.
pBetween :: Char -> Char -> CharParser () a -> CharParser () a
pBetween left right = between (char left) (char right)

-- | Parsers voor bepaalde container types
pListOf, pObjectOf, pTupleOf :: CharParser () a -> CharParser () [a]
pListOf   = pSeries '[' ']'
pObjectOf = pSeries '{' '}'
pTupleOf  = pSeries '(' ')'

-- | een komma-gescheiden container van waarden tussen twee karakters
pSeries :: Char -> Char -> CharParser () a  -> CharParser () [a]
pSeries left right parser = between (char left <* spaces) (spaces *> char right) $
                        (parser <* spaces) `sepBy` (char ',' <* spaces)
