module HelperFunctions where

    import Data.Char (digitToInt)
    import Data.List (genericLength)

    redefineLength :: [Double] -> Double
    redefineLength [] = 0
    redefineLength (_:xs) = 1 + redefineLength xs

    redefineTake :: Double -> [Double] -> [Double]
    redefineTake _ [] = []
    redefineTake i xs | i <= 0 = []
    redefineTake i (x:xs) = x : redefineTake (i-1) xs

    isqrt :: Integer -> Integer 
    isqrt = floor.sqrt.fromIntegral

    splitInteger :: Int -> Int -> [Int]
    splitInteger n s =
        let digits = map digitToInt (show n)
            grouped = chunk s digits
        in map fromDigits grouped
            where
                chunk :: Int -> [a] -> [[a]]
                chunk _ [] = []
                chunk k xs = take k xs : chunk k (drop k xs)
    
    fromDigits :: [Int] -> Int
    fromDigits = foldl (\acc d -> acc * 10 + d) 0

    middle2Digits :: Int -> Int
    middle2Digits n
        | len < 2 = n
        | even len = fromDigits $ take 2 $ drop (len `div` 2 - 1) digits
        | otherwise = fromDigits $ take 2 $ drop (len `div` 2 - 1) digits
            where
                digits = map digitToInt $ show (abs n)
                len = length digits
    
    fractionalPart :: RealFrac a => a -> a
    fractionalPart x = snd (properFraction x)

    extractDouble :: String -> Double
    extractDouble s = read (head (words s)) :: Double