module Hash where

    import RedefinedFunctionsFromPrelude (splitInteger, fromDigits, middle2Digits, fractionalPart)
    import Data.Char (ord)

    moduloHashing :: Int -> Int -> String
    moduloHashing k m
        | m == 0 = error "Can't do modulo by 0"
        | m < 0 = "h(k) = 0"
        | otherwise =
            let h = k `mod` m
            in "h(k) = " ++ show h

    foldingMethod :: Int -> Int -> String
    foldingMethod k m
        | m == 0 = error "Can't do modulo by 0"
        | m < 0 = "h(k) = 0"
        | otherwise =
            let split = sum $ splitInteger k 2
                h = split `mod` m
            in "h(k) = " ++ show h

    midSquareMethod :: Int -> Int -> String
    midSquareMethod k m
        | m == 0 = error "Can't do modulo by 0"
        | m < 0 = "h(k) = 0"
        | otherwise =
            let kSquared = k^2
                middle = middle2Digits kSquared
                h = middle `mod` m
            in "h(k) = " ++ show h
    
    multiplicativeHashing :: Double -> Double -> String
    multiplicativeHashing k m
        | m < 0 = "h(k) = 0"
        | otherwise =
            let a = 0.6180339887
                frac = fractionalPart (k * a)
                h = floor (frac * m)
            in "h(k) = " ++ show h
    
    djb2Hash :: Int -> String -> String
    djb2Hash k s =
        let h = foldl (\acc c -> acc * 33 + ord c) k s
        in "h(k) = " ++ show h

    -- continuation of hash possibly another time xD, next time Polynomial Rolling Hash, Zobrist Hashing, MD5 Hashing and others