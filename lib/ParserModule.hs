module ParserModule where 

import Numeric (readFloat, readHex, readSigned)
import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)
import GHC.IO (unsafePerformIO)
import TypeModule

parseGameFile :: String -> Either ParseError Game
parseGameFile = unsafePerformIO . parseFromFile pGame

pItemList :: CharParser () [Item]
pItemList = pListOf $ between (char '{' <* spaces) (char '}') pItem 

pItem :: CharParser () Item
pItem = Item <$> pListElement (pField pString "id")
             <*> pListElement (pField pNumber "x")
             <*> pListElement (pField pNumber "y")
             <*> pListElement (pField pString "name")
             <*> pListElement (pField pString "description") 
             <*> pListElement (pField pUseTime "useTimes")
             <*> pListElement (pField pNumber "value")
             <*> pListElement (pField pActions "actions")

pEntitieList :: CharParser () [Entity]
pEntitieList = pListOf $ between (char '{' <* spaces) (char '}') pEntity 

pEntity :: CharParser () Entity
pEntity = Entity <$> pListElement (pField pString "id") 
                 <*> pListElement (pField pNumber "x")
                 <*> pListElement (pField pNumber "y")
                 <*> pListElement (pField pString "name")
                 <*> pListElement (pField pString "description")
                 <*> pMaybeListElement (pField pDirection "direction")
                 <*> pMaybeListElement (pField pNumber "hp") 
                 <*> pMaybeListElement (pField pNumber "value") 
                 <*> pListElement (pField pActions "actions")

pMaybeListElement :: CharParser () a -> CharParser () (Maybe a)
pMaybeListElement parser = optionMaybe (try (pListElement parser))

pListElement :: CharParser () a -> CharParser () a
pListElement valueParser = spaces *> valueParser <* (string ",\n" <|> string "\n" <* spaces)

-- | Parser van de volledige game. Ook het ingangspunt van alle parsers.
-- | Deze parser zal worden opgeroepen op het bestand.
pGame :: CharParser () Game
pGame = Game <$> (spaces *> pPlayer) <*> (spaces *> pLevels)

-- | Parsen van meerdere levels
pLevels :: CharParser () [Level]
pLevels = pField (pListOf (pBetween '{' '}' pLevel)) "levels"

-- | Parsen van een enkel level (zonder '{' en '}')
pLevel :: CharParser () Level
pLevel = Level <$> pListElement pLayoutField 
               <*> pListElement pItems 
               <*> pListElement pEntities 

-- | Parsen van een lijst van items.
pItems :: CharParser () [Item]
pItems = pField pItemList "items"

-- | Parsen van een lijst van entities.
pEntities :: CharParser () [Entity]
pEntities = pField pEntitieList "entities"

-- | De mogelijke karakters die een tile in de gameLayout voorstellen.
pWall, pEmpty, pStart, pEnd, pVoid :: CharParser () Char
pWall  = char '*'
pEmpty = char '.'
pStart = char 's'
pEnd   = char 'e'
pVoid  = char 'x'

-- | één enkele tile in de gamelayout
pTile :: CharParser () Char
pTile = pWall <|> pEmpty <|> pStart <|> pEnd <|> pVoid

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
    where hp        = pListElement (pField pNumber "hp") 
          inventory = pListElement (pField pItemList "inventory") 

-- | Meerdere acties parsen
pActions :: CharParser  () [Action]
pActions = pObjectOf pAction

-- | Een enkele functie parsen.
-- | Eerst wordten de conditionele functies geparsed en daarna de actie
pAction :: CharParser () Action
pAction = Action <$> pListOf pFunction <*> (spaces *> pFunction)

-- | Parsen van een functie 
pFunction :: CharParser () Function
pFunction = Function <$> pFunctionName <*> pFunctionArguments

pFunctionName :: CharParser () String
pFunctionName = pCharsButNot ",(]" <* char '(' 

-- | Parsen van functie-argumenten. ofwel opnieuw een functie ofwel ids.
pFunctionArguments :: CharParser () Arguments
pFunctionArguments = (Ids <$> try pIds) <|> (ArgFunction <$> pFunction <* char ')') 

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
pUseTime :: CharParser () (UseTime Int)
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
