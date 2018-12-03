module Part2 where
  import Data.List

  getDifferences :: String -> String -> (String, String, String, String)
  getDifferences s1 s2 = (s1, s2, s1 \\ s2, intersect s1 s2)

  goThroughIds :: [String] -> String
  goThroughIds ls
    | null ls = []
    | length ((head ls) \\ ((head . tail) ls)) == 1 = intersect (head ls) ((head . tail) ls)
    | otherwise = goThroughIds $ tail ls

  answer :: IO ()
  answer = do
    file <- readFile "input.txt"
    let ls = sort $ lines file
    print $ goThroughIds ls