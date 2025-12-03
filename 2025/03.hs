toInt ch = fromIntegral $ fromEnum ch - fromEnum '0' :: Integer
rmLoser (x:(ys@(y:_)))
  | x < y = Just ys
  | Just ys' <- rmLoser ys = Just (x:ys')
rmLoser _ = Nothing
collect xs [] = sum $ zipWith (*) [10^j|j<-[0..]] $ reverse xs
collect xs (y:ys)
  | Just xs' <- rmLoser (xs ++ [y]) = collect xs' ys
  | otherwise = collect xs ys
main = do
  lns <- lines <$> readFile "in/03.txt"
  let ints = map (map toInt) $ lns
  print $ sum $ map (collect [0,0]) ints -- part A
  print $ sum $ map (collect (take 12 $ repeat 0)) ints -- part B
