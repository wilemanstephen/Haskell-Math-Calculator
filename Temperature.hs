module Temperature where

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
        let k = c + 273.15
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
        let r = f + 459.67
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
        let r = k * 9/5
            f = r - 459.67
        in show f ++ " F"

    kelvinToRankine :: Double -> String
    kelvinToRankine k =
        let r = k * 9/5
        in show r ++ " R"
    
    kelvinToRéaumur :: Double -> String
    kelvinToRéaumur k =
        let c = k - 273.15
            re = c * 0.8
        in show re ++ " Ré"
    
    kelvinToRømer :: Double -> String
    kelvinToRømer k =
        let c = k - 273.15
            ro = c * 21/40 + 7.5
        in show ro ++ " Rø"
    
    kelvinToDelisle :: Double -> String
    kelvinToDelisle k =
        let d = (373.15 - k) * 3/2
        in show d ++ " D"

    kelvinToNewton :: Double -> String
    kelvinToNewton k =
        let c = k - 273.15
            n = c * 33/100
        in show n ++ " N"
    
    rankineToCelsius :: Double -> String
    rankineToCelsius r =
        let f = r - 459.67
            c = f * 5/9
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