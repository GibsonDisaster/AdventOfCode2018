module Part1 where
  
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

  run :: [Operation] -> Int -> Int
  run [] x = x
  run ops x = run (drop 1 ops) (apply (head ops) x)

  answer :: IO ()
  answer = do
    file <- readFile "day1\\input.txt"
    let ls = lines file
    let parsed = map parseLine ls
    print (run parsed 0)