module Part2 where
  import Data.List (cycle)
  import qualified Data.Map as M
  import qualified Data.Sequence as S
  
  data Operation = Plus Int | Minus Int | Error deriving (Eq, Show, Ord)

  apply :: Operation -> Int -> Int
  apply (Plus x) y = y + x
  apply (Minus x) y = y - x
  apply Error x = x

  parseLine :: String -> Operation
  parseLine s
    | head s == '+' = Plus (read (drop 1 s))
    | head s == '-' = Minus (read (drop 1 s))
    | otherwise = Error

    {-
  run :: [Operation] -> Int -> Int
  run [] x = x
  run ops x = run (drop 1 ops) (apply (head ops) x) -}

  run :: [Int] -> [Operation] -> Int -> Int
  run pastResults ops x = case ((apply (head ops) x) `elem` pastResults) of
                            True -> (apply (head ops) x)
                            False -> run ([apply (head ops) x] ++ pastResults) (drop 1 ops) ((apply (head ops) x))

  answer :: IO ()
  answer = do
    file <- readFile "day1\\input.txt"
    let ls = lines file
    let parsed = map parseLine ls
    print $ run [] (cycle parsed) 0