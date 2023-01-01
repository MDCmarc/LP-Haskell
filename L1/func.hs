-- entre el 1 y el n no existen mas divisores

absValue :: Int -> Int 
absValue x
    | x>=0 = x
    | otherwise = -x


power :: Int-> Int -> Int
power x p
    | p == 0 = 1
    | otherwise = x * power x (p-1)


isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = not (tieneDivisores 2 )
    where
        tieneDivisores :: Int -> Bool
        tieneDivisores d
            | d*d > x           = False
            | x `mod` d == 0    = True
            | otherwise         = tieneDivisores(d+1)

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1)+slowFib(n-2)

quickFib :: Int -> Int
quickFib 0 = 0
quickFib 1 = 1
quickFib n = fib 0 1 1
    where
        fib :: Int -> Int -> Int -> Int
        fib f s x
            | x == n = s
            | otherwise = fib s (f+s) (x+1)
