module Css
  ( Selector,
    Key,
    Val,
    Rule (..),
    RuleSet (..),
    selectorParser,
    keyParser,
    ruleParser,
    ruleSetParser,
  )
where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String

-- Define the basic Type
type Selector = String

type Key = String

type Val = String

data Rule = Rule Key Val deriving (Show)

data RuleSet = RuleSet Selector [Rule] deriving (Show)

-- Define the basic Parser

selectorParser :: Parser Selector
selectorParser =
  many1 (oneOf ".#" <|> letter <|> digit)
    <* spaces

keyParser :: Parser Key
keyParser =
  many1 letter
    <* spaces
    <* char ':'
    <* spaces

valParser :: Parser Val
valParser =
  many1 (noneOf ";")
    <* char ';'
    <* spaces

rulesParser :: Parser [Rule]
rulesParser =
  char '{'
    <* spaces
    *> many1 ruleParser
    <* char '}'
    <* spaces

ruleParser :: Parser Rule
ruleParser = do
  p <- keyParser
  v <- valParser
  return $ Rule p v

ruleSetParser :: Parser RuleSet
ruleSetParser = do
  s <- selectorParser `sepBy1` spaces
  r <- rulesParser
  return $ RuleSet (unwords s) r
