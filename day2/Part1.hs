module Part1 where
  import Data.List

  hasDouble :: String -> Int
  hasDouble s = case length (filter (\x -> length x == 2) ((group . sort) s)) >= 1 of
                  True -> 1
                  False -> 0

  hasTriple :: String -> Int
  hasTriple s = case length (filter (\x -> length x == 3) ((group . sort) s)) >= 1 of
                  True -> 1
                  False -> 0

  answer :: IO ()
  answer = do
    file <- readFile "test.txt"
    let ls = lines file
    let allDoubles = sum $ map hasDouble ls
    let allTriples = sum $ map hasTriple ls
    print $ allDoubles * allTriples