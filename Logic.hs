module Logic where

    -- not, and, or are already defined in Haskell Prelude model, thus I will not redefine them here

    implies :: Bool -> Bool -> Bool
    implies p q = not p || q

    equivalence :: Bool -> Bool -> Bool
    equivalence p q = p == q
    
    xor :: Bool -> Bool -> Bool
    xor p q = not $ equivalence p q
