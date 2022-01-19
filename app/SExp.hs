module SExp
  ( SVal (..),
    listParser,
  )
where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

data SVal
  = SNum Double
  | SBool Bool
  | SStr String
  | SNil
  | SList [SVal]
  | SAtom String
  deriving (Show, Eq)

nilParser :: Parser SVal
nilParser = SNil <$ string "nil"

atomParser :: Parser SVal
atomParser = SAtom <$> many1 (letter <|> oneOf "+-*/><")

trueParser :: Parser SVal
trueParser = SBool <$> (True <$ string "#t")

falseParser :: Parser SVal
falseParser = SBool <$> (False <$ string "#f")

boolParser :: Parser SVal
boolParser = try trueParser <|> try falseParser

strParser :: Parser SVal
strParser =
  SStr
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

numParser :: Parser SVal
numParser = SNum . read <$> numStrParser

valParser :: Parser SVal
valParser =
  nilParser
    <|> boolParser
    <|> atomParser
    <|> strParser
    <|> numParser
    <|> listParser

listParser :: Parser SVal
listParser =
  SList
    <$> ( char '('
            *> spaces
            *> sepBy valParser spaces
            <* spaces
            <* char ')'
        )
