import Data.List(groupBy)
((.*.) `on` f) x y = f x .*. f y
isDigit ch = ch >= '0' && ch <= '9'
alternate [] = []
alternate (x:_:rest) = x : alternate rest
pairUp [] = []
pairUp (x:y:rest) = (x,y) : pairUp rest
divisors reps = [ (factor,factor*10^(p-1),factor*(10^p-1))
                | p <- [1..10], q <- [1..reps]
                , let factor = sum [ 10^(p*t) | t <- [0..q] ] ]
getDiv part n = [ factor | (factor,min,max) <- part, n >= min, n <= max ]
isInvalid part n = any ((0==).(n `mod`)) $ getDiv part n
findInvalid part (a,z) = [ n | n <- [a..z], isInvalid part n ]
sumInvalid part = sum . (findInvalid part)
main = do
  ln <- readFile "in/02.txt"
  let ranges = pairUp $ map read $ (alternate $ groupBy ((==) `on` isDigit) ln) :: [(Integer,Integer)]
  print $ sum $ map (sumInvalid $ divisors 1) ranges -- part A
  print $ sum $ map (sumInvalid $ divisors 6) ranges -- part B
