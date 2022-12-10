module VoorbeeldModule where

import Numeric (readFloat, readHex, readSigned)
import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)
import System.IO
import Control.Concurrent (waitQSem)
import GHC.Read (list)

parse :: String -> IO GameLayout 
parse fileName = parseFromFile pText fileName >>= either report return
  where report err = exitFailure

main :: IO ()
main = do
     result <- parseFromFile pText "levels/test1.txt"
     case result of
       Left err     -> print err
       Right output -> print output

data GameObjectField = Id String 
                | X Int
                | Y Int
                | Hp Int
                | Name String
                | Description String     
                | UseTimes UseTime
                | ObjectValue Int
                | Inventory [GameObject]
                | GameActions [GameAction]
                deriving (Eq, Show)

data UseTime = Finite Int | Infinite deriving (Eq, Show)

type Name = String
type GameId = String
data Arguments = Function GameFunction | GameIds [GameId]
    deriving (Eq, Show)

-- TODO Record syntax?
data GameFunction = GameFuntion Name Arguments 
    deriving (Eq, Show)

type GameObject = [GameObjectField]

type Conditions = [GameFunction]
type Action = GameFunction
data GameAction = GameAction Conditions Action 
    deriving (Eq, Show)

pGameObjectField :: CharParser () GameObjectField
pGameObjectField = value <* spaces
    where value = Id          <$> pField pString "id"
              <|> X           <$> pField pNumber "x"
              <|> Y           <$> pField pNumber "y"
              <|> Hp          <$> pField pNumber "hp"
              <|> Name        <$> pField pString "name"
              <|> Description <$> pField pString "description"
              <|> UseTimes    <$> pField pUseTime "useTimes"
              <|> ObjectValue <$> pField pNumber "value"
              <|> GameActions <$> pField pGameActions "actions"
              <|> Inventory   <$> pField pInventory "inventory"
              <?> "GameObjectField"

type PlayerAttributes = [GameObjectField]
data GamePlayer = Player PlayerAttributes deriving (Eq, Show)

data GameLayout = Layout [[Char]] deriving (Eq, Show)

pWall, pEmpty, pStart, pEnd :: CharParser () Char
pWall  = char '*'
pEmpty = char '.'
pStart = char 's'
pEnd   = char 'e'

pLayoutField :: CharParser () GameLayout
pLayoutField = spaces *> pField (between (char '{') (char '}') pLayout) "layout"

pLayoutObject :: CharParser () Char
pLayoutObject = pWall <|> pEmpty <|> pStart <|> pEnd

pLayoutRow :: CharParser () [Char]
pLayoutRow = pLayoutObject `sepBy` char ' '

pLayout :: CharParser () GameLayout
pLayout = Layout <$> (delim *> (pLayoutRow `sepBy` try delim) <* fDelim)
    where delim = fDelim *> string "| "
          fDelim = char '\n' *> many (char ' ')

pPlayer :: CharParser () GamePlayer
pPlayer = Player <$> pField (pObjectOf pGameObjectField) "player"

pGameObjects :: CharParser () [GameObject]
pGameObjects = pListOf pGameObject 

pGameObject :: CharParser () GameObject
pGameObject = pObjectOf pGameObjectField

pInventory :: CharParser () [GameObject]
pInventory = pListOf pGameObject

pGameActions :: CharParser  () [GameAction]
pGameActions = pObjectOf pGameAction

pGameAction :: CharParser () GameAction
pGameAction = GameAction <$> pListOf pGameFunction <*> (spaces *> pGameFunction)

pGameFunction :: CharParser () GameFunction
pGameFunction = GameFuntion <$> pFunctionName <*> pFunctionArguments

pFunctionName :: CharParser () String
pFunctionName = pCharsButNot ",(]" <* char '(' 

pFunctionArguments :: CharParser () Arguments
pFunctionArguments = (GameIds <$> try pGameIds) <|> (Function <$> pGameFunction <* char ')') 

pGameIds :: CharParser () [String]
pGameIds = (pSingleGameId `sepBy` char ',') <* char ')'

pSingleGameId :: CharParser () String
pSingleGameId  = spaces *> pCharsButNot [',', ')', ' ', '('] <* spaces

pCharsButNot :: [Char] -> CharParser () String
pCharsButNot excluded = many1 $ satisfy (`notElem` excluded)

-- JSON tekst bestaat uit ofwel een object ofwel een array, met eventueel whitespace
pText :: CharParser () GameLayout 
pText = pLayoutField

pField :: CharParser () a -> String -> CharParser () a
pField valueParser key = try $ (string key >> char ':' >> spaces) >> valueParser

pString :: CharParser () String
pString = between (char '\"') (char '\"') (pCharsButNot ['"'])

pUseTime :: CharParser () UseTime
pUseTime = (Infinite <$ string "infinite") <|> (Finite <$> pNumber)

readInt :: String -> Int
readInt = read

pNumber :: CharParser () Int
pNumber = readInt <$> many1 digit

pListOf, pObjectOf, pTupleOf :: CharParser () a -> CharParser () [a]
pListOf   = pSeries '[' ']'
pObjectOf = pSeries '{' '}'
pTupleOf  = pSeries '(' ')'

pSeries :: Char -> Char -> CharParser () a  -> CharParser () [a]
pSeries left right parser = between (char left <* spaces) (char right) $
                        (parser <* spaces) `sepBy` (char ',' <* spaces)
