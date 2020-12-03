#! /usr/bin/env runhaskell

import System.IO
import Data.Char
import Text.ParserCombinators.Parsec

data Rule = Rule {c :: Char,
                  lower :: Int,
                  upper :: Int,
                  password :: String} deriving (Show)

passwordFile :: GenParser Char st [Rule]
passwordFile = do
  result <- many rule
  eof
  return result

rule :: GenParser Char st Rule
rule = do
  lower <- number
  char '-'
  upper <- number
  space
  c <- letter
  string ": "
  password <- many letter
  eol
  return (Rule c lower upper password)

number :: GenParser Char st Int
number = do
  number <- many digit
  return $ read number


eol :: GenParser Char st Char
eol = char '\n'

parseRules :: String -> Either ParseError [Rule]
parseRules input = parse passwordFile "(unknown)" input

valid :: Rule -> Bool
valid (Rule c a b password) = lowerPresent /= upperPresent -- xor
      where lowerPresent = password !! (a - 1) == c
            upperPresent = password !! (b - 1) == c

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  file <- readFile "input"
  let rules = parseRules file
  case rules of
    Right rules -> print $ Main.count valid rules
    Left error -> print error
