module Logic where

    import Data.List (nub)

    -- not, and, or are already defined in Haskell Prelude model, thus I will not redefine them here

    implies :: Bool -> Bool -> Bool
    implies p q = not p || q

    equivalence :: Bool -> Bool -> Bool
    equivalence p q = p == q

    xor :: Bool -> Bool -> Bool
    xor p q = not $ equivalence p q

    data Prop =
        Var String
        | Not Prop
        | And Prop Prop
        | Or Prop Prop
        | Implies Prop Prop
        | Equiv Prop Prop
        | Xor Prop Prop
        deriving (Eq, Show)

    type Env = [(String, Bool)]

    eval :: Env -> Prop -> Bool
    eval env (Var x) = case lookup x env of
        Just val -> val
        Nothing -> error ("Variable " ++ x ++ " not found")
    eval env (Not p) = not (eval env p)
    eval env (And p q) = eval env p && eval env q
    eval env (Or p q) = eval env p || eval env q
    eval env (Implies p q) = not (eval env p) || eval env q
    eval env (Equiv p q) = eval env p == eval env q
    eval env (Xor p q) = eval env p /= eval env q

    vars :: Prop -> [String]
    vars (Var x) = [x]
    vars (Not p) = vars p
    vars (And p q) = vars p ++ vars q
    vars (Or p q) = vars p ++ vars q
    vars (Implies p q) = vars p ++ vars q
    vars (Equiv p q) = vars p ++ vars q
    vars (Xor p q) = vars p ++ vars q

    uniqueVars :: Prop -> [String]
    uniqueVars = nub.vars

    envs :: [String] -> [Env]
    envs [] = [[]]
    env (v:vs) = [(v, val) : e | val <- [False, True], e <- envs vs]

    isTautology :: Prop -> Bool
    isTautology p =
        let vs = uniqueVars p
            allEnvs = envs vs
        in all (`eval` p) allEnvs
    
    isContradiction :: Prop -> Bool
    isContradiction p = 
        let vs = nub (vars p)
            allEnvs = envs vs
        in all (\env -> not (eval env p)) allEnvs
    
    isContingent :: Prop -> Bool
    isContingent p = not (isTautology p) && not (isContradiction p)

    transformToCNF :: Env -> Prop
    transformToCNF [] = Var "True"
    transformToCNF env = foldr1 Or [if val then Not (Var v) else Var v | (v, val) <- env]

    transformToDNF :: Env -> Prop
    transformToDNF [] = Var "True"
    transformToDNF env = foldr1 And [if val then Var v else Not (Var v) | (v, val) <- env]

    truthTable :: Prop -> IO ()
    truthTable p = do
        let vs = nub (vars p)
            es = envs vs
        putStrLn $ unwords vs ++ " | Result"
        putStrLn $ replicate (length vs * 2 + 9) '-'
        mapM_ (printRow vs) es
        where
            printRow :: [String] -> Env -> IO ()
            printRow vs env =
                let values = [showBool (lookup v env) | v <- vs]
                    result = showBool (Just (eval env p))
                in putStrLn $ unwords values ++ " | " ++ result
                where
                    showBool :: Maybe Bool -> String
                    showBool (Just True) = "T"
                    showBool (Just False) = "F"
                    showBool Nothing = error "We either need a truth value or a false value"