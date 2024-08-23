eval1 :: String -> Int
eval1 expr =  calcula (words expr) []

calcula :: [String] -> [Int] -> Int
calcula [] [x] = x
calcula (x:expr) pila
    |x == "+" =  calcula expr $ [newVal (+)]   ++ (drop 2 pila) 
    |x == "-" =  calcula expr $ [newVal (-)]   ++ (drop 2 pila)
    |x == "*" =  calcula expr $ [newVal (*)]   ++ (drop 2 pila)  
    |x == "/" =  calcula expr $ [newVal (div)] ++ (drop 2 pila)
    |otherwise = calcula expr $ [read x] ++ pila 

    where
        newVal:: (Int->Int->Int)->Int
        newVal f = f (pila!!1) (pila!!0)

-------------------------------P.3---------------------------

fsmap :: a -> [a -> a] -> a
fsmap x listf = foldl (flip ($)) x listf

----------------------------- P.4 ------------------------

divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b

