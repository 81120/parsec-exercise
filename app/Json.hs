module Json
  ( JVal (..),
    nullParser,
    boolParser,
    stringParser,
    numParser,
    arrayParser,
    jsonParser,
    objectParser,
  )
where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

data JVal
  = JNum Double
  | JBool Bool
  | JString String
  | JObject [Pair]
  | JArray [JVal]
  | JNull
  deriving (Show, Eq)

newtype Key = Key String deriving (Show, Eq)

data Pair = Pair Key JVal deriving (Show, Eq)

nullParser :: Parser JVal
nullParser = JNull <$ string "null"

trueParser :: Parser JVal
trueParser = JBool <$> (True <$ string "true")

falseParser :: Parser JVal
falseParser = JBool <$> (False <$ string "false")

boolParser :: Parser JVal
boolParser = try trueParser <|> try falseParser

stringParser :: Parser JVal
stringParser =
  JString
    <$> ( char '"'
            *> many1 (letter <|> space)
            <* char '"'
        )

floatStrParser :: Parser String
floatStrParser = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ x ++ "." ++ y

intStrParser :: Parser String
intStrParser = many1 digit

numStrParser :: Parser String
numStrParser = try floatStrParser <|> try intStrParser

numParser :: Parser JVal
numParser = JNum . read <$> numStrParser

jsonParser :: Parser JVal
jsonParser =
  nullParser
    <|> boolParser
    <|> numParser
    <|> stringParser
    <|> arrayParser

arrayParser :: Parser JVal
arrayParser =
  JArray
    <$> ( char '['
            *> sepBy jsonParser (char ',')
            <* char ']'
        )

keyParser :: Parser Key
keyParser =
  Key
    <$> ( char '"'
            *> many1 letter
            <* char '"'
        )

pairParser :: Parser Pair
pairParser = do
  key <- keyParser
  Pair key <$> (char ':' *> jsonParser)

objectParser :: Parser JVal
objectParser =
  JObject
    <$> ( char '{'
            *> sepBy pairParser (char ',')
            <* char '}'
        )
