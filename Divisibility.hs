module Divisibility where

    import  HelperFunctions (isqrt)

    isPrime :: Integer -> Bool
    isPrime n
        | n <= 1 = False
        | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]

    primeList :: [Integer]
    primeList = [x | x <- [2..], isPrime x]

    checkDivisibility :: Integer -> Integer -> Bool
    checkDivisibility x y = y `mod` x == 0

    isCongruent :: Integer -> Integer -> Integer -> Bool
    isCongruent a b n
        | a <= 0 && b <= 0 = error "Number can't de divisible with something less than or equal to 0"
        | n == 0 = error "modulo 0 is undefined"
        | checkDivisibility n (a-b) = True
        | otherwise = False

    findGCD :: Integer -> Integer -> Integer
    findGCD x y
        | x < y = findGCD y x
        | x < 0 || y < 0 = findGCD (abs x) (abs y)
        | y == 0 = abs x
        | otherwise = findGCD y (x `mod` y)

    checkCoPrime :: Integer -> Integer -> Bool
    checkCoPrime x y = findGCD x y == 1

    findLCM :: Integer -> Integer -> Integer
    findLCM x y
        | x < y = findLCM y x
        | x < 0 || y < 0 = findLCM (abs x) (abs y)
        | y == 0 = error "Impossible to do division by 0"
        | findGCD x y == 0 = error "Impossible to do division by 0"
        | otherwise = (x*y) `div` findGCD x y

    bezoutIdentity :: Integer -> Integer -> (Integer, Integer, Integer)
    bezoutIdentity a b
        | a < 0 || b < 0 = bezoutIdentity (abs a) (abs b)
        | b == 0 = (a, 1, 0)
        | otherwise =
            let (d, x1, y1) = bezoutIdentity b (a `mod` b)
                q = a `div` b
                x = y1
                y = x1 - q * y1
            in (d, x, y)