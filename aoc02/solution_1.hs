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

makeRule :: String -> Maybe(Rule)
makeRule (a:'-':b:' ':c:':':' ':p) = Just $ Rule c (digitToInt a) (digitToInt b) p
makeRule _ = Nothing

valid :: Rule -> Bool
valid (Rule c lower upper password) = occ >= lower && occ <= upper
      where occ = length $ filter (\x -> x == c) $ password

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  file <- readFile "input"
  let rules = parseRules file
  case rules of
    Right rules -> print $ Main.count valid rules
    Left error -> print error
