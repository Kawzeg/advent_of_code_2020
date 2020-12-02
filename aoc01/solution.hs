import System.IO

main :: IO ()
main = do
  file <- readFile "input"
  let input = map read $ lines file :: [Int]
  print $ f 2 input
  print $ f 3 input

-- | Gets all combinations of a certain size of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 []        = [[]]
combinations _ []        = []
combinations 0 _         = [[]]
combinations size (x:xs) = [x:ps | ps <- combinations (size - 1) xs] ++ combinations size xs

f :: Int -> [Int] -> Int
f x = foldl1 (*) . head . filter (\xs -> 2020 == sum xs) . combinations x
