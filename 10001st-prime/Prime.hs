
hasFactors :: [Int] -> Int -> Bool
hasFactors ps n = or $ map ((==0) . mod n) ps

-- This is inefficient: no point checking if factor of 7 for 11 etc
-- Think about improving this to reduce num of comparisons.
enumPrimes :: [Int]
enumPrimes = enumPrimes' [] 2
  where
    enumPrimes' ps n
      | hasFactors ps n = enumPrimes' ps (n+1)
      | otherwise = n : enumPrimes' (n:ps) (n+1)

nthPrime :: Int -> Int
nthPrime n = enumPrimes !! (n-1)
