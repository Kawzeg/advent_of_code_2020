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
      slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]
      trees = map (\(a,b) -> length $ elemIndices '#' $ ride 0 a b input) slopes
      result = foldl1 (*) trees
  print result
