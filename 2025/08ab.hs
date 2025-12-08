import Data.List(sort)
import qualified Data.Map as M
import qualified Data.Set as S
pairs [] = []
pairs (x:ys) = [ (x,y) | y<-ys ] ++ pairs ys
dist (x,y,z) (x',y',z') = square (x-x') + square (y-y') + square (z-z')
  where square a = a * a
addPair (boxToRep, repToSet) (a,b) = (boxToRep', repToSet')
  where a' = boxToRep M.! a
        b' = boxToRep M.! b
        boxToRep' = M.union (M.fromList [(x,a')|x<-S.toList b'Set]) boxToRep
        repToSet' = M.insertWith S.union a' b'Set $ M.delete b' repToSet
        b'Set = repToSet M.! b'
partB boxToRep repToSet (((a@(ax,_,_)),(b@(bx,_,_))):rest)
  | M.size repToSet' == 1 = ax * bx
  | otherwise = partB boxToRep' repToSet' rest
  where (boxToRep', repToSet') = addPair (boxToRep, repToSet) (a,b)
main = do
  lns <- lines <$> readFile "in/08.txt"
  let boxes = [ read $ '(' : ln ++ ")" | ln <- lns ] :: [(Int,Int,Int)]
      nearest = map snd $ sort [ (dist a b, (a,b)) | (a,b) <- pairs boxes ]
      boxToRep = M.fromList [ (b,b) | b <- boxes ]
      repToSet = M.fromList [ (b, S.singleton b) | b <- boxes ]
      (_, partA) = foldl addPair (boxToRep, repToSet) $ take 1000 nearest
  print $ product $ take 3 $ reverse $ sort $ map S.size $ M.elems partA
  print $ partB boxToRep repToSet nearest
