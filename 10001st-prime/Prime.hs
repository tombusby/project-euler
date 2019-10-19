
hasFactors :: [Int] -> Int -> Bool
hasFactors xs n = or $ map ((==0) . mod n) xs

enumPrimes :: [Int]
enumPrimes = enumPrimes' [] 2
  where
    enumPrimes' ps n
      | hasFactors ps n = enumPrimes' ps (n+1)
      | otherwise = n : enumPrimes' (n:ps) (n+1)

nthPrime :: Int -> Int
nthPrime n = enumPrimes !! (n-1)
