{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geometry where
    import Data.List (sort)

    newtype Angles = Angles Double deriving (Show, Eq, Ord, Num, Fractional, Floating)

    getAngle :: Angles -> Double
    getAngle (Angles x) = x

    checkIfValidAngle :: Angles -> Bool
    checkIfValidAngle ang
        | ang <= 0 = False
        | otherwise = True

    checkIfAcuteAngle :: Angles -> Bool
    checkIfAcuteAngle ang
        | not (checkIfValidAngle ang) = False
        | ang < pi/2 = True
        | otherwise = False

    checkIfRightAngle :: Angles -> Bool
    checkIfRightAngle ang
        | not (checkIfValidAngle ang) = False
        | ang == pi/2 = True
        | otherwise = False

    checkIfObtuseAngle :: Angles -> Bool
    checkIfObtuseAngle ang
        | not (checkIfValidAngle ang) = False
        | ang > pi/2 && ang <= pi = True
        | otherwise = False

    checkIfStraightAngle :: Angles -> Bool
    checkIfStraightAngle ang
        | not (checkIfValidAngle ang) = False
        | ang == pi = True
        | otherwise = False

    checkIfReflexAngle :: Angles -> Bool
    checkIfReflexAngle ang
        | not (checkIfValidAngle ang) = False
        | ang > pi && ang < 2*pi = True
        | otherwise = False

    data Triangle = Triangle {
        sideA :: Double,
        sideB :: Double,
        sideC :: Double,
        angleA :: Angles,
        angleB :: Angles,
        angleC :: Angles
    } deriving Show

    checkIfValidTriangle :: Triangle -> Bool
    checkIfValidTriangle t
        | a <= 0 && b <= 0 && c <= 0 = False
        | (bac + abc + bca > 180) && (bac <= 0 || abc <= 0 || bca <= 0) = False
        | otherwise = True
        where
            a = sideA t
            b = sideB t
            c = sideC t
            bac = angleA t
            abc = angleB t
            bca = angleC t

    checkIfRightTriangle :: Triangle -> Bool
    checkIfRightTriangle t
        | not (checkIfValidTriangle t) = False
        | (bac == pi/2 || abc == pi/2 || bca == pi/2) && hypotenuse**2 == leg1**2 + leg2**2 = True
        | otherwise = False
        where
            [leg1, leg2, hypotenuse] = sort [sideA t, sideB t, sideC t]
            bac = angleA t
            abc = angleB t
            bca = angleC t

    checkIfEquilateralTriangle :: Triangle -> Bool
    checkIfEquilateralTriangle t
        | not (checkIfValidTriangle t) = False
        | (bac == pi/3 && abc == pi/3 && bca == pi/3) && (a == b && b == c && a == c) = True
        | otherwise = False
        where
            a = sideA t
            b = sideB t
            c = sideC t
            bac = angleA t
            abc = angleB t
            bca = angleC t

    checkIfIsoscelesTriangle :: Triangle -> Bool
    checkIfIsoscelesTriangle t
        | not (checkIfValidTriangle t) = False
        | (bac == abc || bac == bca || abc == bca) && (a == b || b == c || a == c) = True
        | otherwise = False
        where
            a = sideA t
            b = sideB t
            c = sideC t
            bac = angleA t
            abc = angleB t
            bca = angleC t

    checkIfRightIsoscelesTriangle :: Triangle -> Bool
    checkIfRightIsoscelesTriangle t
        | not (checkIfValidTriangle t) = False
        | checkIfRightTriangle t && checkIfIsoscelesTriangle t = True
        | otherwise = False

    checkIfScaleneTriangle :: Triangle -> Bool
    checkIfScaleneTriangle t
        | not (checkIfValidTriangle t) = False
        | (a /= b && a /= c && b /= c) && (bac /= abc && bac /= bca && abc /= bca) = True
        | otherwise = False
        where
            a = sideA t
            b = sideB t
            c = sideC t
            bac = angleA t
            abc = angleB t
            bca = angleC t

    perimeterOfTriangle :: Triangle -> Double
    perimeterOfTriangle t
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | otherwise = a + b + c
        where
            a = sideA t
            b = sideB t
            c = sideC t

    areaOfTriangleUsingHeight :: Triangle -> Double -> Double
    areaOfTriangleUsingHeight t h
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | otherwise = 1/2 * (base * h)
        where
            base = sideB t

    heronsFormulaArea :: Triangle -> Double
    heronsFormulaArea t
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | otherwise =
            let s = perimeterOfTriangle t / 2
            in sqrt (s * (s-a) * (s-b) * (s-c))
        where
            a = sideA t
            b = sideB t
            c = sideC t

    areaOfTriangleUsingAngle :: Triangle -> Double
    areaOfTriangleUsingAngle t
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | otherwise = 1/2 * (a*b) * sin (getAngle bca)
        where
            a = sideA t
            b = sideB t
            bca = angleC t

    heightFromPoint :: Triangle -> Double -> (Double, Double, Double)
    heightFromPoint t area
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | otherwise =
            let hA = (2*area)/a
                hB = (2*area)/b
                hC = (2*area)/c
            in (hA, hB, hC)
        where
            a = sideA t
            b = sideB t
            c = sideC t

    inradiusOfTriangle :: Triangle -> Double
    inradiusOfTriangle t
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | otherwise =
            let s = perimeterOfTriangle t / 2
                area = heronsFormulaArea t
                r = area/s
            in r

    circumradiusOfTriangle :: Triangle -> Double
    circumradiusOfTriangle t
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | checkIfRightTriangle t = hypotenuse / 2
        | otherwise = (a * b * c) / (4 * areaOfTriangleUsingAngle t)
        where
            a = sideA t
            b = sideB t
            c = sideC t
            hypotenuse = maximum [a, b, c]
    

