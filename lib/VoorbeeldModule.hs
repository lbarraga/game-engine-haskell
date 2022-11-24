module VoorbeeldModule where

import Numeric (readFloat, readHex, readSigned)
import Text.ParserCombinators.Parsec
import System.Exit (exitFailure)
import System.IO

parse :: String -> IO (String, JValue)
parse fileName = parseFromFile pText fileName >>= either report return
  where report err = exitFailure

main :: IO ()
main = do
     result <- parseFromFile pText "levels/level1.txt"
     case result of
       Left err     -> print err
       Right output -> print output

newtype JAry a = JAry { fromJAry :: [a] } 
    deriving (Eq, Ord, Show)

newtype JObj a = JObj { fromJObj :: [(String, a)] } 
    deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JField (String, JValue)
            | JObject [(String, JValue)]
            | JArray  [JValue]
            | JKey String
            deriving (Eq, Ord, Show)

-- JSON tekst bestaat uit ofwel een object ofwel een array, met eventueel whitespace
pText :: CharParser () (String, JValue)
pText = spaces *> text <?> "JSON text"
    where text =  pField


pField :: CharParser () (String,  JValue)
pField  = (,) <$> (pKey <* char ':' <* spaces) <*> pValue

pObject :: CharParser () [(String, JValue)]
pObject = pSeries '{' pField '}'

pArray :: CharParser () [JValue]
pArray = pSeries '[' pValue ']'

pValue :: CharParser () JValue
pValue = value <* spaces
  where value = JString <$> pString
            <|> JNumber <$> pNumber
            <|> JObject <$> pObject
            <|> JArray  <$> pArray
            <|> JBool   <$> pBool
            <|> JNull   <$  string "null"
            <?> "JSON value"

pBool :: CharParser () Bool
pBool = True  <$ string "true"
    <|> False <$ string "false"

pValueChoice = value <* spaces
  where value = choice [ JString <$> pString
                       , JNumber <$> pNumber
                       , JObject <$> pObject
                       , JArray  <$> pArray
                       , JBool   <$> pBool
                       , JKey    <$> pKey
                       , JNull   <$ string "null"
                       ]
                <?> "JSON value"

pKey :: CharParser () String
pKey = many $ satisfy (`notElem` ":\"")

pString :: CharParser () String
pString = between (char '\"') (char '\"') (many jchar)
    where jchar = satisfy (`notElem` "\"")

decode :: Char -> Char -> CharParser () Char
decode c r = r <$ char c

pNumber :: CharParser () Double
pNumber = do 
    s <- getInput
    case readSigned readFloat s of
              [(n, s')] -> n <$ setInput s'
              _         -> pzero

pSeries :: Char -> CharParser () a -> Char -> CharParser () [a]
pSeries left parser right = between (char left <* spaces) (char right) $
                        (parser <* spaces) `sepBy` (char ',' <* spaces)

