
isPalindrome :: Show a => a -> Bool
isPalindrome n = show n == reverse (show n)

max3DigitProductPalindromes :: (Integral a, Show a) => a
max3DigitProductPalindromes = let ns = [999,998..100] in
  head $ filter isPalindrome [x * y | x <- ns, y <- ns]
