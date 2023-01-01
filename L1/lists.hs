myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [last] = last
myMaximum (x:xs) = mymax x (myMaximum xs)
    where
        mymax :: Int -> Int -> Int
        mymax a b 
            | a >= b = a 
            | otherwise = b

average :: [Int] -> Float
average a = fromIntegral (mysum a) / fromIntegral(myLength a)
    where 
    mysum :: [Int] -> Int
    mysum [] = 0
    mysum (x:xs) = x + mysum xs

buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome list = (reverse list) ++ (list)
    where 
        reverse :: [Int] -> [Int]
        reverse [last] = [last]
        reverse (x:xs) = (reverse xs) ++ [x]

remove :: [Int] -> [Int] -> [Int]
remove a [] = a
remove [] _ = []              
remove (a:as) b = (erase a b) ++ (remove as b)
    where 
        erase :: Int -> [Int] -> [Int]
        erase a [] = [a] 
        erase a (b:bs) 
            | a == b = []
            | otherwise = erase a bs


flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = (x) ++ (flatten xs )


oddsNevens:: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens a = oddsNevens2 a [] []
    where 
        oddsNevens2 :: [Int] -> [Int] -> [Int]  -> ([Int],[Int])
        oddsNevens2 [] odds evens = (odds,evens)
        oddsNevens2 (a:as) odds evens 
            | even a = oddsNevens2 as odds (evens ++ [a])
            | otherwise  = oddsNevens2 as (odds ++ [a]) evens
            
oNe (x:xs)
        |even x = (odds , x:evens)
        | ow = (x:odds, evens)
    where (odds , evens) = oNe xs





primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors n = get_divisors 2 []
    where 
        get_divisors :: Int -> [Int] -> [Int]
        get_divisors p list
            -- hasta n+1 xq sino n mod n == 0 y no se incluiria en la sol
            | n+1 == p = list
            | n `mod` p == 0 && isPrime p = get_divisors (p+1) ((list)++[p])
            |otherwise = get_divisors (p+1) list
            where 
                isPrime :: Int -> Bool
                isPrime 0 = False
                isPrime 1 = False
                isPrime x = not (tieneDivisores 2 )
                    where
                        tieneDivisores :: Int -> Bool
                        tieneDivisores d
                            -- optimizacion ya que los divisores han de ser <=sqrt(x)
                            | d*d > x           = False
                            | x `mod` d == 0    = True
                            | otherwise         = tieneDivisores(d+1)