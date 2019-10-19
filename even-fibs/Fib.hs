
fibs :: [Integer]
fibs = 0 : 1 : fib' 0 1
  where
    fib' n m = let next = n + m in next : fib' m next

-- "starting with 1 and 2":
-- Zero doesn't change the result, extra 1 is filtered by even
sumEvenFibsUnder4Mil :: Integer
sumEvenFibsUnder4Mil = sum . filter even $ takeWhile (<=4000000) fibs
