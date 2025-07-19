module HelperFunctions where

    import Data.Char (digitToInt)
    import Data.List (genericLength, sort)

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

    conversionFromListToTupleWith2Elems :: Ord a => [a] -> (a, a)
    conversionFromListToTupleWith2Elems [x, y] = 
        let [a, b] = sort [x, y]
        in (a, b)

    sum2ElemTuple :: (Double, Double) -> Double
    sum2ElemTuple (x, y) = x + y
    
    checkIf2ndElemBiggerInTuple2Elem :: (Double, Double) -> Bool
    checkIf2ndElemBiggerInTuple2Elem (x, y)
        | x < y = True
        | otherwise = False