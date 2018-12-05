module Part1 where
  import Data.List
  -- Get the amount of area that is overlapped by all the fabric squares
  -- #1 @ 82,901: 26x12
  -- 1000x1000 area

  data FabricSquare = FSArea Int [(Int, Int)] -- ID Area-Covered-By-Fabric-Square
                    | FabricSquare Int Int Int Int Int -- ID Left Top Width Height
                    deriving (Show, Eq, Ord)

  parseFS :: String -> FabricSquare
  parseFS s = FabricSquare i left top width height
    where
      ws = words s
      i = read $ (drop 1 . head) ws -- good
      left = read $ takeWhile (/= ',') ((head . drop 2) ws) -- still ok
      top = read $ reverse (takeWhile (/= ',') ((tail . reverse . head . drop 2) ws)) -- wtf this sucks
      width = read $ takeWhile (/= 'x') ((head . drop 3) ws) -- fine
      height = read $ reverse (takeWhile (/= 'x') ((reverse . head . drop 3) ws)) -- i give up

  makeArea :: Int -> Int -> Int -> Int -> [(Int, Int)]
  makeArea x y w h = [(x', y')| x' <- [x..x+w-1], y' <- [y..y+h-1]]

  makeFS :: FabricSquare -> FabricSquare
  makeFS (FabricSquare i x y w h) = FSArea i $ makeArea x y w h

  findOverlaps :: [FabricSquare] -> [(Int, Int)]
  findOverlaps fss
    | null fss || length fss == 1 = [] -- may not give right answer
    | otherwise = ((\(FSArea i as) (FSArea i' as') -> intersect as as') (head fss) ((head . drop 1) fss)) ++ (findOverlaps $ tail fss)

  findOverlaps' :: [FabricSquare] -> [(Int, Int)]
  findOverlaps' fss
    | null fss || length fss == 1 = []
    | otherwise = (concat (map (inter (head fss)) (tail fss) )) ++ (findOverlaps $ tail fss)
      where
        inter = (\(FSArea i as) (FSArea i' as') -> intersect as as')

  answer :: IO ()
  answer = do
    file <- readFile "input.txt"
    let ls = lines file
    let parsed = map parseFS ls
    let converted = map makeFS parsed
    print $ length (findOverlaps' converted)