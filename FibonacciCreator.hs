module FibonacciCreator where

    fibonacciNumber :: Integer -> Integer
    fibonacciNumber 0 = 0
    fibonacciNumber 1 = 1
    fibonacciNumber x = fibonacciNumber (x-1) + fibonacciNumber (x-2)

    fibonacciList :: [Integer] -> [Integer]
    fibonacciList xs = [fibonacciNumber x | x <- xs]