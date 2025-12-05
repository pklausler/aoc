import Data.List(sort)
getRange ln = (read a, read z) :: (Integer, Integer)
  where (a,(_:z)) = break ('-'==) ln
rangeSize (a,z) = z - a + 1
merge ((a,b):(rest@((c,d):rest')))
  | b < c = (a,b) : merge rest
  | otherwise = merge ((a,d `max` b):rest')
merge last = last
main = do
  lns <- lines <$> readFile "in/05.txt"
  let (rangeLines, (_:itemLines)) = break null lns
      fresh = merge $ sort $ filter ((0<).rangeSize) $ map getRange rangeLines
      isFresh n = any inRange fresh
        where inRange (a,z) = n >= a && n <= z
  print $ length $ filter (isFresh . read) itemLines -- part A
  print $ sum $ map rangeSize $ fresh -- part B
