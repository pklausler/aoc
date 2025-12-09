import qualified Data.Map as M
import qualified Data.Set as S
pairs [] = []
pairs (a:bs) = [ (a,b) | b <- bs ] ++ pairs bs
size ((x1,y1),(x2,y2)) = (abs (x1-x2)+1) * (abs (y1-y2)+1)
cmprss xs = M.fromList $ zip (S.toList $ S.fromList xs) [0,2..]
compress xys = M.fromList [ ((xcm M.! x, ycm M.! y), (x,y)) | (x,y)<-xys ]
  where (xcm,ycm) = (cmprss $ map fst xys, cmprss $ map snd xys)
rectangle (x1,y1) (x2,y2) = S.fromList [(x,y)|x<-[min x1 x2 .. max x1 x2], y<-[min y1 y2 .. max y1 y2]]
trace (xy:(rest@(xy':_))) = S.union (rectangle xy xy') $ trace rest
trace _ = S.empty
flood _ soFar [] = soFar
flood border soFar ((xy@(x,y)):rest)
  | xy `S.member` border = flood border soFar rest
  | xy `S.member` soFar = flood border soFar rest
  | otherwise = flood border (S.insert xy soFar) $ halo ++ rest
  where halo = [ (x,y+1), (x,y-1), (x+1,y), (x-1,y) ]
main = do
  lns <- lines <$> readFile "in/09.txt"
  let tiles = [ read $ '(':ln++")" | ln <- lns ] :: [(Int,Int)]
      uncmpTileMap = compress tiles
      cmpTileMap = M.fromList [ (c,u) | (u,c) <- M.toList uncmpTileMap ]
      cTiles = map (cmpTileMap M.!) tiles
      borderSet = trace $ cTiles ++ [head cTiles]
      border = S.toList borderSet
      (maxX,maxY) = (maximum $ map fst border, maximum $ map snd border)
      frame = S.unions [rectangle (-2,-2) (maxX+2,-2),
                        rectangle (maxX+2,-2) (maxX+2,maxY+2),
                        rectangle (maxX+2,maxY+2) (-2,maxY+2),
                        rectangle (-2,maxY+2) (-2,-2)]
      outside = flood (S.union borderSet frame) S.empty [(-1,-1)]
      cRects = [ box | (box@(xy1,xy2)) <- pairs cTiles
                     , S.null $ outside `S.intersection` rectangle xy1 xy2 ]
      rects = [ (uncmpTileMap M.! xy1, uncmpTileMap M.!  xy2)
              | (xy1,xy2) <- cRects ]
  print $ maximum $ map size $ pairs tiles -- part A
  print $ maximum $ map size $ rects -- part B
