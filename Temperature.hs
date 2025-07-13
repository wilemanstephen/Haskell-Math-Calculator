module Temperature where

    import HelperFunctions (extractDouble)

    celsiusToFahrenheit :: Double -> String
    celsiusToFahrenheit c =
        let f = c * 9/5 + 32
        in show f ++ " F"
    
    celsiusToKelvin :: Double -> String
    celsiusToKelvin c =
        let k = c + 273.15
        in show k ++ " K"
    
    celsiusToRankine :: Double -> String
    celsiusToRankine c =
        let k = extractDouble $ celsiusToKelvin c
            r = k * 9/5   
        in show r ++ " R"

    celsiusToRéaumur :: Double -> String
    celsiusToRéaumur c =
        let re = c * 0.8
        in show re ++ " Ré"

    celsiusToRømer :: Double -> String
    celsiusToRømer c =
        let ro = c * 21/40 + 7.5
        in show ro ++ " Rø"
    
    celsiusToDelisle :: Double -> String
    celsiusToDelisle c =
        let d = (100 - c) * 3/2
        in show d ++ " D"
    
    celsiusToNewton :: Double -> String
    celsiusToNewton c =
        let n = c * 33/100
        in show n ++ " N"
    
    fahrenheitToCelsius :: Double -> String
    fahrenheitToCelsius f =
        let c = (f - 32) * 5/9
        in show c ++ " C"

    fahrenheitToKelvin :: Double -> String
    fahrenheitToKelvin f =
        let r = extractDouble $ fahrenheitToRankine f
            k = r * 5/9
        in show k ++ " K"
    
    fahrenheitToRankine :: Double -> String
    fahrenheitToRankine f =
        let r = f + 459.67
        in show r ++ " R"
    
    fahrenheitToRéaumur :: Double -> String
    fahrenheitToRéaumur f =
        let re = (f - 32) * 4/9
        in show re ++ " Ré"

    fahrenheitToRømer :: Double -> String
    fahrenheitToRømer f =
        let ro = (f - 32) * 7/24 + 7.5
        in show ro ++ " Rø"

    fahrenheitToDelisle :: Double -> String
    fahrenheitToDelisle f =
        let d = (212 - f) * 5/6
        in show d ++ " D"
    
    fahrenheitToNewton :: Double -> String
    fahrenheitToNewton f =
        let n = (f - 32) * 11/60
        in show n ++ " N"

    kelvinToCelsius :: Double -> String
    kelvinToCelsius k =
        let c = k - 273.15
        in show c ++ " C"
    
    kelvinToFahrenheit :: Double -> String
    kelvinToFahrenheit k =
        let r = extractDouble $ kelvinToRankine k
            f = r - 459.67
        in show f ++ " F"

    kelvinToRankine :: Double -> String
    kelvinToRankine k =
        let r = k * 9/5
        in show r ++ " R"
    
    kelvinToRéaumur :: Double -> String
    kelvinToRéaumur k =
        let c = extractDouble $ kelvinToCelsius k
            re = c * 0.8
        in show re ++ " Ré"
    
    kelvinToRømer :: Double -> String
    kelvinToRømer k =
        let c = extractDouble $ kelvinToCelsius k
            ro = c * 21/40 + 7.5
        in show ro ++ " Rø"
    
    kelvinToDelisle :: Double -> String
    kelvinToDelisle k =
        let d = (373.15 - k) * 3/2
        in show d ++ " D"

    kelvinToNewton :: Double -> String
    kelvinToNewton k =
        let c = extractDouble $ kelvinToCelsius k
            n = c * 33/100
        in show n ++ " N"
    
    rankineToCelsius :: Double -> String
    rankineToCelsius r =
        let c = (r - 491.67) * 5/9
        in show c ++ " C"
    
    rankineToFahrenheit :: Double -> String
    rankineToFahrenheit r =
        let f = r - 459.67
        in show f ++ " F"

    rankineToKelvin :: Double -> String
    rankineToKelvin r =
        let k = r * 5/9
        in show k ++ " K"
    
    rankineToRéaumur :: Double -> String
    rankineToRéaumur r =
        let re = (r - 491.67) * 4/9
        in show re ++ " Ré"
    
    rankineToRømer :: Double -> String
    rankineToRømer r =
        let ro = (r - 491.67) * 7/24 + 7.5
        in show ro ++ " Rø"
    
    rankineToDelisle :: Double -> String
    rankineToDelisle r =
        let d = (671.67 - r) * 5/6
        in show d ++ " D"
    
    rankineToNewton :: Double -> String
    rankineToNewton r =
        let n = (r - 491.67) * 11/60
        in show n ++ " N"

    réaumurToCelsius :: Double -> String
    réaumurToCelsius re =
        let c = re * 1.25
        in show c ++ " C"
    
    réaumurToFahrenheit :: Double -> String
    réaumurToFahrenheit re =
        let f = re * 2.25 + 32
        in show f ++ " F"
    
    réaumurToKelvin :: Double -> String
    réaumurToKelvin re =
        let c = extractDouble $ réaumurToCelsius re
            k = c + 273.15
        in show k ++ " K"
    
    réaumurToRankine :: Double -> String
    réaumurToRankine re =
        let k = extractDouble $ réaumurToKelvin re
            r = k * 9/5
        in show r ++ " R"
    
    réaumurToRømer :: Double -> String
    réaumurToRømer re =
        let ro = re * 21/32 + 7.5
        in show ro ++ " Rø"
    
    réaumurToDelisle :: Double -> String
    réaumurToDelisle re =
        let c = extractDouble $ réaumurToCelsius re
            d = (100 - c) * 3/2
        in show d ++ " D"
    
    réaumurToNewton :: Double -> String
    réaumurToNewton re =
        let n = re * 33/80
        in show n ++ " N"
    
    rømerToCelsius :: Double -> String
    rømerToCelsius ro =
        let c = (ro - 7.5) * 40/21
        in show c ++ " C"
    
    rømerToFahrenheit :: Double -> String
    rømerToFahrenheit ro =
        let f = (ro - 7.5) * 24/7 + 32
        in show f ++ " F"
    
    rømerToKelvin :: Double -> String
    rømerToKelvin ro =
        let c = extractDouble $ rømerToCelsius ro
            k = c + 273.15
        in show k ++ " K"

    rømerToRankine :: Double -> String
    rømerToRankine ro =
        let r = (ro - 7.5) * 72/21 + 491.67
        in show r ++ " R"
    
    rømerToRéaumur :: Double -> String
    rømerToRéaumur ro =
        let re = (ro - 7.5) * 32/21
        in show re ++ " Ré"
    
    rømerToDelisle :: Double -> String
    rømerToDelisle ro =
        let c = extractDouble $ rømerToCelsius ro
            d = (100 - c) * 3/2
        in show d ++ " D"
    
    rømerToNewton :: Double -> String
    rømerToNewton ro =
        let n = (ro - 7.5) * 33/40
        in show n ++ " N"
    
    delisleToCelsius :: Double -> String
    delisleToCelsius d =
        let c = 100 - d * 2/3
        in show c ++ " C"

    delisleToFahrenheit :: Double -> String
    delisleToFahrenheit d =
        let f = 212 - d * 1.2
        in show f ++ " F"

    delisleToKelvin :: Double -> String
    delisleToKelvin d =
        let k = 373.15 - d * 2/3
        in show k ++ " K"
    
    delisleToRankine :: Double -> String
    delisleToRankine d =
        let r = 671.67 - d * 1.2
        in show r ++ " R"
    
    delisleToRéaumur :: Double -> String
    delisleToRéaumur d =
        let c = extractDouble $ delisleToCelsius d
            re = c * 0.8
        in show re ++ " Ré"
    
    delisleToRømer :: Double -> String
    delisleToRømer d =
        let c = extractDouble $ delisleToCelsius d
            ro = c * 21/40 + 7.5
        in show ro ++ " Rø"
    
    delisleToNewton :: Double -> String
    delisleToNewton d =
        let n = celsiusToNewton $ extractDouble $ delisleToCelsius d
        in show n ++ " N"
    
    newtonToCelsius :: Double -> String
    newtonToCelsius n =
        let c = n * 100/33
        in show c ++ " C"

    newtonToFahrenheit :: Double -> String
    newtonToFahrenheit n =
        let f = n * 60/11 + 32
        in show f ++ " F"

    newtonToKelvin :: Double -> String
    newtonToKelvin n =
        let k = celsiusToKelvin $ extractDouble $ newtonToCelsius n
        in show k ++ " K"
    
    newtonToRankine :: Double -> String
    newtonToRankine n =
        let k = extractDouble $ newtonToKelvin n
            r = k * 9/5
        in show r ++ " R"

    newtonToRéaumur :: Double -> String
    newtonToRéaumur n =
        let re = n * 80/33
        in show re ++ " Ré"
    
    newtonToRømer :: Double -> String
    newtonToRømer n =
        let ro = n * 40/33 + 7.5
        in show ro ++ " Rø"
    
    newtonToDelisle :: Double -> String
    newtonToDelisle n =
        let c = extractDouble $ newtonToCelsius n
            d = (100 - c) * 3/2
        in show d ++ " D"