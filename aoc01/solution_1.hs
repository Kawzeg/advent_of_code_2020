import System.IO

main :: IO ()
main = do
  input <- readFile "input"
  let result = f $ map read $ lines input
  print result

-- | Gets all combinations of a certain size of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 []        = [[]]
combinations _ []        = []
combinations 0 _         = [[]]
combinations size (x:xs) = [x:ps | ps <- combinations (size - 1) xs] ++ combinations size xs

f :: [Int] -> Int
f xs = do
  let cs = combinations 2 xs
  head [a*b | (a:b:_) <- cs, a + b==2020]
