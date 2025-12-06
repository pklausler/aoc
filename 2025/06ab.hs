import Data.List(transpose,groupBy)
op "+" = (+) :: (Integer -> Integer -> Integer)
op "*" = (*)
op x = error x
chunk [] = []
chunk lns = (init (head c) : tail c) : (chunk $ drop 1 rest)
  where (c,rest) = break (all (==' ')) lns
main = do
  lns <- lines <$> readFile "in/06.txt" -- "t.txt"
  let ops = map op $ words $ last lns
      partA = transpose $ map words $ init lns
      partB = chunk $ transpose lns
  print $ sum [ foldr1 o $ map read xs | (o,xs) <- zip ops partA ]
  print $ sum [ foldr1 o $ map read xs | (o,xs) <- zip ops partB ]
