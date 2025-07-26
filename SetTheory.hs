module SetTheory where

    import Data.List (nub, sort)
    import HelperFunctions (checkIfSameList, checkIfAllElems)

    isElemSet :: Eq a => a -> [a] -> Bool
    isElemSet _ [] = False
    isElemSet e (x:xs) 
        | e `elem` (x:xs) = True
        | otherwise = False
    
    checkIfSubset :: (Eq a, Ord a) => [a] -> [a] -> Bool
    checkIfSubset [] _ = True
    checkIfSubset _ [] = False
    checkIfSubset (x:xs) (y:ys)
        | checkIfSameList (nub (x:xs)) (nub (y:ys)) = True
        | length (x:xs) <= length (y:ys) && checkIfAllElems (nub (x:xs)) (nub (y:ys)) = True
        | otherwise = False


    union :: Eq a => [a] -> [a] -> [a]
    union [] l2 = nub l2
    union l1 [] = nub l1
    union l1 l2 = nub (l1 ++ l2)

    intersection :: Eq a => [a] -> [a] -> [a]
    intersection [] _ = []
    intersection _ [] = []
    intersection (x:xs) (y:ys) = if isElemSet x (y:ys) then nub $ x : intersection xs (y:ys) else intersection xs (y:ys)

    difference :: (Eq a, Ord a) => [a] -> [a] -> [a]
    difference [] _ = []
    difference (x:xs) [] = nub (x:xs)
    difference (x:xs) (y:ys) 
        | checkIfSameList (nub (x:xs)) (nub (y:ys)) = []
        | isElemSet x (y:ys) = difference xs (y:ys)
        | otherwise = nub (x : difference xs (y:ys))
    
    cardinality :: [a] -> Int
    cardinality = length
    
    complement :: (Eq a, Num a, Enum a) => [a] -> [a]
    complement xs = [x | x <- [0..], x `notElem` xs]

    symmetricDifference :: (Eq a, Ord a) => [a] -> [a] -> [a]
    symmetricDifference xs ys = sort $ nub $ difference xs ys ++ difference ys xs