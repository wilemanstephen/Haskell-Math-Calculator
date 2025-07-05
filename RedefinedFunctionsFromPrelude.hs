module RedefinedFunctionsFromPrelude where

    redefineLength :: [Double] -> Double
    redefineLength [] = 0
    redefineLength (_:xs) = 1 + redefineLength xs

    redefineTake :: Double -> [Double] -> [Double]
    redefineTake _ [] = []
    redefineTake i xs | i <= 0 = []
    redefineTake i (x:xs) = x : redefineTake (i-1) xs

    isqrt :: Integer -> Integer 
    isqrt = floor.sqrt.fromIntegral