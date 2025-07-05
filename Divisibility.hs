module Divisibility where

    import RedefinedFunctionsFromPrelude

    isPrime :: Integer -> Bool
    isPrime n
        | n <= 1 = False
        | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]
    
    primeList :: [Integer]
    primeList = [x | x <- [2..], isPrime x]