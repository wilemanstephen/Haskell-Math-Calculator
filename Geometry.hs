{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geometry where

    import Data.List (sort)
    import HelperFunctions (conversionFromListToTupleWith2Elems, sum2ElemTuple, checkIf2ndElemBiggerInTuple2Elem)

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
        | ang < pi/2 || ang < 90 = True
        | otherwise = False

    checkIfRightAngle :: Angles -> Bool
    checkIfRightAngle ang
        | not (checkIfValidAngle ang) = False
        | ang == pi/2 || ang == 90 = True
        | otherwise = False

    checkIfObtuseAngle :: Angles -> Bool
    checkIfObtuseAngle ang
        | not (checkIfValidAngle ang) = False
        | (ang > pi/2 && ang <= pi) || (ang > 90 && ang <= 180) = True
        | otherwise = False

    checkIfStraightAngle :: Angles -> Bool
    checkIfStraightAngle ang
        | not (checkIfValidAngle ang) = False
        | ang == pi || ang == 180 = True
        | otherwise = False

    checkIfReflexAngle :: Angles -> Bool
    checkIfReflexAngle ang
        | not (checkIfValidAngle ang) = False
        | (ang > pi && ang < 2*pi) || (ang > 180 && ang < 360) = True
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
        | any (<= 0) [a, b, c] = False
        | (bac + abc + bca > 180 || bac + abc + bca > pi) && any (<= 0) [bac, abc, bca] = False
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
        | (elem (pi/2) [bac, abc, bca] || elem 90 [bac, abc, bca]) && hypotenuse**2 == leg1**2 + leg2**2 = True
        | otherwise = False
        where
            [leg1, leg2, hypotenuse] = sort [sideA t, sideB t, sideC t]
            bac = angleA t
            abc = angleB t
            bca = angleC t

    checkIfEquilateralTriangle :: Triangle -> Bool
    checkIfEquilateralTriangle t
        | not (checkIfValidTriangle t) = False
        | (all (== pi/3) [bac, abc, bca] || all (== 60) [bac, abc, bca]) && (a == b && b == c && a == c) = True
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

    areaOfRightTriangle :: Triangle -> Double
    areaOfRightTriangle t
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | not (checkIfRightTriangle t) = error " For this formula to work, the triangle needs to be a right angled triangle"
        | otherwise = (leg1 * leg2)/2
        where
            [leg1, leg2] = take 2 (sort [sideA t, sideB t, sideC t])

    heightFromPoint :: Triangle -> (Double, Double, Double)
    heightFromPoint t
        | not (checkIfValidTriangle t) = error "Triangle with negative or no side length at all does not exist"
        | otherwise =
            let area = heronsFormulaArea t
                hA = (2*area)/a
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

    data Parallelogram = Parallelogram {
        lengthP :: Double,
        widthP :: Double,
        angleACP :: Angles,
        angleBDP :: Angles
    } deriving Show

    checkIfParallelogram :: Parallelogram -> Bool
    checkIfParallelogram p
        | any (<= 0) [l, w] = False
        | any (< 0) [bac, bca]  = False
        | bac + bca /= pi = False
        | 2 * bac + 2 * bca /= 2*pi = False
        | otherwise = True
        where
            l = lengthP p
            w = widthP p
            bac = angleACP p
            bca = angleBDP p

    perimeterOfParallelogram :: Parallelogram -> Double
    perimeterOfParallelogram p
        | not (checkIfParallelogram p) = error "Not a parallelogram, just a 4 sided regular shape, or non-existent shape"
        | otherwise = 2 * (l + w)
        where
            l = lengthP p
            w = widthP p

    areaOfParallelogramUsingHeight :: Parallelogram -> Double -> Double
    areaOfParallelogramUsingHeight p h
        | not (checkIfParallelogram p) = error "Not a parallelogram, just a 4 sided regular shape, or non-existent shape"
        | otherwise = base * h
        where
            base = max (lengthP p) (widthP p)

    areaOfParallelogramUsingAngle1 :: Parallelogram -> Double
    areaOfParallelogramUsingAngle1 p
        | not (checkIfParallelogram p) = error "Not a parallelogram, just a 4 sided regular shape, or non-existent shape"
        | otherwise = a * b * sin (getAngle abc)
        where
            a = lengthP p
            b = widthP p
            abc = angleBDP p

    areaOfParallelogramUsingAngle2 :: Parallelogram -> Double
    areaOfParallelogramUsingAngle2 p
        | not (checkIfParallelogram p) = error "Not a parallelogram, just a 4 sided regular shape, or non-existent shape"
        | otherwise = a * b * sin (getAngle bca)
        where
            a = lengthP p
            b = widthP p
            bca = angleACP p

    findDiagonalsParallelogram :: Parallelogram -> (Double, Double)
    findDiagonalsParallelogram p
        | not (checkIfParallelogram p) = error "Not a parallelogram, just a 4 sided regular shape, or non-existent shape"
        | otherwise =
            let d1 = sqrt (l**2 + w**2 + 2*l*w*cos (getAngle abc))
                d2 = l * w * 2*l*w*cos (getAngle bca)
            in conversionFromListToTupleWith2Elems $ sort [d1, d2]
        where
            l = lengthP p
            w = widthP p
            [abc, bca] = sort [angleACP p, angleBDP p]

    data Rectangle = Rectangle {
        widthR :: Double,
        lengthR :: Double,
        angleR :: Angles
    }

    checkIfRectangle :: Rectangle -> Bool
    checkIfRectangle r
        | ang /= 90 || ang /= pi/2 = False
        | l /= w = True
        | otherwise = False
        where
            ang = angleR r
            l = lengthR r
            w = widthR r

    perimeterOfRectangle :: Rectangle -> Double
    perimeterOfRectangle r
        | not (checkIfRectangle r) = error "Not a valid rectangle"
        | otherwise = 2*(l+w)
        where
            l = lengthR r
            w = widthR r

    areaOfRectangle :: Rectangle -> Double
    areaOfRectangle r
        | not (checkIfRectangle r) = error "Not a valid rectangle"
        | otherwise = l * w
        where
            l = lengthR r
            w = widthR r

    findDiagonalsRectangle :: Rectangle -> Double
    findDiagonalsRectangle r
        | not (checkIfRectangle r) = error "Not a valid rectangle"
        | otherwise = sqrt (l**2 + w**2)
        where
            l = lengthR r
            w = widthR r

    circumradiusOfRectangle :: Rectangle -> Double
    circumradiusOfRectangle r
        | not (checkIfRectangle r) = error "Not a valid rectangle"
        | otherwise = findDiagonalsRectangle r / 2

    data Rhombus = Rhombus {
        sideRh :: Double,
        angle1Rh :: Angles,
        angle2Rh :: Angles
    }

    checkIfRhombus :: Rhombus -> Bool
    checkIfRhombus rh
        | dab == abc = False
        | checkIf2ndElemBiggerInTuple2Elem $ diagonalsOfRhombus rh = True
        | otherwise = False
        where
            dab = angle1Rh rh
            abc = angle2Rh rh

    perimeterOfRhombus :: Rhombus -> Double
    perimeterOfRhombus rh
        | not (checkIfRhombus rh) = error "Not valid rhombus"
        | otherwise = 4 * sideRh rh

    findHeightRhombus :: Rhombus -> Double
    findHeightRhombus rh
        | not (checkIfRhombus rh) = error "Not valid rhombus"
        | otherwise = sideRh rh * sin (getAngle dab)
        where
            dab = angle1Rh rh

    areaOfRhombusUsingHeight :: Rhombus -> Double
    areaOfRhombusUsingHeight rh
        | not (checkIfRhombus rh) = error "Not valid rhombus"
        | otherwise = sideRh rh * findHeightRhombus rh

    diagonalsOfRhombus :: Rhombus -> (Double, Double)
    diagonalsOfRhombus rh
        | not (checkIfRhombus rh) = error "Not valid rhombus"
        | otherwise =
            let d1 = 2 * sideRh rh * sin (getAngle dab / 2)
                d2 = 2 * sideRh rh * cos (getAngle dab / 2)
            in conversionFromListToTupleWith2Elems $ sort [d1, d2]
        where
            dab = angle1Rh rh

    areaOfRhombusUsingDiagonals :: Rhombus -> Double
    areaOfRhombusUsingDiagonals rh
        | not (checkIfRhombus rh) = error "Not valid rhombus"
        | otherwise = 1/2 * sum2ElemTuple (diagonalsOfRhombus rh)

    areaOfRhombusUsingAngles :: Rhombus -> Double
    areaOfRhombusUsingAngles rh
        | not (checkIfRhombus rh) = error "Not valid rhombus"
        | otherwise = sideRh rh ^ 2 * sin (getAngle dab)
        where
            dab = angle1Rh rh

    data Square = Square {
        sideS :: Double,
        angleS :: Angles
    }

    checkIfSquare :: Square -> Bool
    checkIfSquare s
        | angleS s == pi/2 || angleS s == 90 = True
        | otherwise = False

    perimeterOfSquare :: Square -> Double
    perimeterOfSquare s
        | not (checkIfSquare s) = error "Not a valid square"
        | otherwise = 4 * sideS s

    areaOfSquareUsingSides :: Square -> Double
    areaOfSquareUsingSides s
        | not (checkIfSquare s) = error "Not a valid square"
        | otherwise = sideS s ** 2

    areaOfSquareUsingDiagonals :: Square -> Double
    areaOfSquareUsingDiagonals s
        | not (checkIfSquare s) = error "Not a valid square"
        | otherwise = findDiagonalsSquare s ** 2 / 2

    findDiagonalsSquare :: Square -> Double
    findDiagonalsSquare s
        | not (checkIfSquare s) = error "Not a valid square"
        | otherwise = sideS s * sqrt 2

    inradiusOfSquare :: Square -> Double
    inradiusOfSquare s
        | not (checkIfSquare s) = error "Not a valid square"
        | otherwise = sideS s / 2

    circumradiusOfSquareUsingSides :: Square -> Double
    circumradiusOfSquareUsingSides s
        | not (checkIfSquare s) = error "Not a valid square"
        | otherwise = sideS s / sqrt 2

    circumradiusOfSquareUsingDiagonals :: Square -> Double
    circumradiusOfSquareUsingDiagonals s
        | not (checkIfSquare s) = error "Not a valid square"
        | otherwise = findDiagonalsSquare s / 2

    data Trapezium = Trapezium {
        legATr :: Double,
        legBTr :: Double,
        bottomBaseTr :: Double,
        upperBaseTr :: Double,
        angleATr :: Angles,
        angleBTr :: Angles,
        angleCTr :: Angles,
        angleDTr :: Angles
    }

    checkIfValidTrapezium :: Trapezium -> Bool
    checkIfValidTrapezium tr
        | b1 == b2 = False
        | any (> 180) [dab, abc, bcd, cda] && any (< 0) [dab, abc, bcd, cda] = False
        | dab + abc /= 180 && bcd + cda /= 180 = False
        | otherwise = True
        where
            b1 = upperBaseTr tr
            b2 = bottomBaseTr tr
            dab = angleATr tr
            abc = angleBTr tr
            bcd = angleCTr tr
            cda = angleDTr tr
    checkIfIsoscelesTrapezium :: Trapezium -> Bool
    checkIfIsoscelesTrapezium tr
        | not (checkIfValidTrapezium tr) = False
        | leg1 /= leg2 = False
        | dab /= cda && abc /= bcd = False
        | otherwise = True
        where
            leg1 = legATr tr
            leg2 = legBTr tr
            dab = angleATr tr
            abc = angleBTr tr
            bcd = angleCTr tr
            cda = angleDTr tr

    checkIfRightTrapezium :: Trapezium -> Bool
    checkIfRightTrapezium tr
        | not (checkIfValidTrapezium tr) = False
        | 90 `elem` [dab, abc, bcd, cda] || pi/2 `elem` [dab, abc, bcd, cda] = True
        | otherwise = False
        where
            dab = angleATr tr
            abc = angleBTr tr
            bcd = angleCTr tr
            cda = angleDTr tr

    checkIfIsoscelesRightTrapezium :: Trapezium -> Bool
    checkIfIsoscelesRightTrapezium tr
        | not (checkIfValidTrapezium tr) = False
        | checkIfIsoscelesTrapezium tr && checkIfRightTrapezium tr = True
        | otherwise = False
