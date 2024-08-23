sumMultiplies35:: Integer -> Integer
sumMultiplies35 n =  foldl (+) 0 [x | x<-[1..n-1], x `mod`3 == 0 || x `mod` 5 == 0]


fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fib 0 1 1
    where
        fib :: Int -> Int -> Int -> Integer
        fib f s x
            | x == n = toInteger s
            | otherwise = fib s (f+s) (x+1)


sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = foldl (+) 0 (filter even $ takeWhile (<n) fibs)


fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


largestPrimeFactor:: Int -> Int
largestPrimeFactor n

isPalindromic :: Integer -> Bool
isPalindromic n = foldl (&&) True $ zipWith (==) (show n) (reverse $ show n)