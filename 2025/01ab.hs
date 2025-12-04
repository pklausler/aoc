mapLR ('L':n) = - read n
mapLR ('R':n) = read n
partA (h,at) turn = (h+hit0,at')
  where at' = (at + turn) `mod` 100
        hit0 = fromEnum $ at' == 0
partB (p,at) turn = (p+np,at')
  where (d,at') = divMod (at + turn) 100
        np | turn < 0, at == 0 = abs d - 1
           | turn < 0, at' == 0 = abs d + 1
           | otherwise = abs d
main = do
  lns <- lines <$> readFile "in/01.txt"
  let turns = mapLR <$> lns
  print $ fst $ foldl partA (0,50) turns
  print $ fst $ foldl partB (0,50) turns
