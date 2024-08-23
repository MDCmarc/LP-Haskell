data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val a) = a
eval1 (Add a b) = (eval1 a) + (eval1 b)
eval1 (Sub a b) = (eval1 a) -  (eval1 b)
eval1 (Mul a b) = (eval1) a * (eval1 b)
eval1 (Div a b) = (eval1 a) `div` (eval1 b)

eval2 :: Expr -> Maybe Int
eval2 (Val a) = return a
eval2 (Add a b) = eval2' (+) a b
eval2 (Sub a b) = eval2' (-) a b
eval2 (Mul a b) = eval2' (*) a b
eval2 (Div a b) = eval2div' a b


eval2' :: (Int->Int->Int)->Expr->Expr->Maybe Int
eval2' f a b = do 
    i <- eval2 a
    j <- eval2 b
    Just (f i j)

eval2div' :: Expr->Expr->Maybe Int
eval2div' a b = do 
        i <- eval2 a
        j <- eval2 b
        dividir i j 


dividir ::  Int ->  Int -> Maybe Int 
dividir a b 
    | b == 0 = Nothing
    | otherwise = Just (div a b)


eval3 :: Expr -> Either String Int
eval3 (Val a) = Right a
eval3 (Add a b) = eval3' (+) a b
eval3 (Sub a b) = eval3' (-) a b
eval3 (Mul a b) = eval3' (*) a b
eval3 (Div a b) = eval3div' a b


eval3' :: (Int->Int->Int)->Expr->Expr->Either String Int
eval3' f a b = do 
    i <- eval3 a
    j <- eval3 b
    Right (f i j)

eval3div' :: Expr->Expr->Either String Int
eval3div' a b = do 
        i <- eval3 a
        j <- eval3 b
        dividir3 i j 


dividir3 ::  Int ->  Int -> Either String Int
dividir3 a b 
    | b == 0 = Left "div0"
    | otherwise = Right (div a b)