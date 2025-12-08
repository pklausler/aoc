import Data.List(elemIndices)
import qualified Data.Map as M
import qualified Data.Set as S
partA _ [] = 0
partA beams (s:ss) = S.size splits + partA beams' ss
  where splits = S.intersection beams s
        beams' = S.unions [ beams S.\\ splits, S.map (subtract 1) splits, S.map (+1) splits ]
partB times s = M.fromList [ (at, f at ct) | (at,ct) <- M.toList times ]
  where f at ct = (if at `S.member` s then 0 else ct) +
                  (if (at - 1) `S.member` s then times M.! (at - 1) else 0) +
                  (if (at + 1) `S.member` s then times M.! (at + 1) else 0)
main = do
  lns <- lines <$> readFile "in/07.txt"
  let [start] = elemIndices 'S' $ head lns
      splits = map (S.fromList . elemIndices '^') $ tail lns
      zeroes = M.fromList $ take (length $ head lns) $ zip [0..] $ repeat 0
      table = M.insert start 1 zeroes
  print $ partA (S.singleton start) splits
  print $ sum $ M.elems $ foldl partB table splits
