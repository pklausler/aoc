import qualified Data.Set as S
neighbors set (r,c) = sum [ 1 | r'<-[r-1..r+1], c'<-[c-1..c+1]
                              , r'/=r || c'/=c, S.member (r',c') set ]
accessible set = S.filter ((4>) . neighbors set) set
partB n set
  | S.null got = n
  | otherwise = partB (n + S.size got) $ set S.\\ got
  where got = accessible set
main = do
  lns <- lines <$> readFile "in/04.txt"
  let set = S.fromList [ (r,c) | (r,ln) <- zip [1..] lns
                               , (c,ch) <- zip [1..] ln, ch == '@' ]
  print $ S.size $ accessible set -- part A
  print $ partB 0 set
