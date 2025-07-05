module Sums where

    import RedefinedFunctionsFromPrelude
    import FibonacciCreator
    import Divisibility

    gaussianSum :: Integer -> Integer -> Integer
    gaussianSum 0 k = 0
    gaussianSum x k = x^k + gaussianSum (x-1) k

    gaussianSumList :: [Integer] -> Integer -> Integer
    gaussianSumList xs k = sum [x^k | x <- xs]

    arithmeticSum :: Double -> Double -> Double -> Double
    arithmeticSum s n d
        | s == n && d == 0 = s
        | otherwise = (n/2) * (2*s + (n-1)*d)

    arithmeticSumList :: [Double] -> Double -> Double -> Double
    arithmeticSumList xs n d
        | redefineLength xs == n && d == 0 = sum xs
        | otherwise = (n/2) * (minimum (redefineTake n xs) + maximum (redefineTake n xs))

    geometricSum :: Double -> Double -> Double -> Double
    geometricSum a n r
        | r == 0 = a
        | r == 1 = a * n
        | otherwise = a * ((1 - r**n)/(1 - r))

    geometricSumList :: [Double] -> Double -> Double -> Double
    geometricSumList xs n r
        | r == 0 = head xs
        | r == 1 = head xs * n
        | otherwise = head xs * ((1 - r**n)/(1 - r))

    harmonicSum :: Double -> Integer -> Double
    harmonicSum 0 _ = error "Division by 0 impossible"
    harmonicSum 1 _ = 1
    harmonicSum h k = (1/h^k) + harmonicSum (h-1) k

    harmonicSumList :: [Double] -> Integer -> Double
    harmonicSumList xs k
        | head xs == 0 = error "Division by 0 impossible"
        | otherwise = sum [1/x^k | x <- xs]

    alternatingSeriesSum :: Double -> Double
    alternatingSeriesSum 0 = error "Division by 0 impossible"
    alternatingSeriesSum alt = ((-1)**(alt + 1))/alt

    triangularNumbers :: Integer -> Integer
    triangularNumbers 1 = 1
    triangularNumbers x = x + triangularNumbers (x-1)

    triangularNumbersList :: [Integer] -> Integer
    triangularNumbersList = sum

    fibonacciSum :: Integer -> Integer
    fibonacciSum 0 = 0
    fibonacciSum 1 = 1
    fibonacciSum x = fibonacciNumber x + fibonacciSum (x-1)

    fibonacciSumList :: [Integer] -> Integer
    fibonacciSumList xs = sum $ fibonacciList xs

    sumOfPrimes :: Integer -> Integer
    sumOfPrimes x
        | x < 2 = error "No prime number smaller than 2 exists"
        | x == 2 = 2
        | not (isPrime x) = sumOfPrimes (x-1)
        | isPrime x = x + sumOfPrimes (x-1)
    
    sumOfPrimesList :: Int -> Integer
    sumOfPrimesList n = sum $ take n primeList