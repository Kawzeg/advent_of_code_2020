import System.IO
import Data.List

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery x (first:xs) = first : (takeEvery x rest)
  where rest = drop (x - 1) xs

ride :: Int -> Int -> Int -> [[a]] -> [a]
ride _ _      down xs | down > length xs = []
ride x across down xs = (head xs) !! x : ride x' across down rest
  where rest = drop down xs
        x' = (x + across) `mod` (length $ head xs)

main :: IO ()
main = do
  file <- readFile "input"
  let input = lines file
      result = length $ elemIndices '#' $ ride 0 3 1 input
  print result
