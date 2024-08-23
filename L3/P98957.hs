ones :: [Integer]
--ones = repeat 1
ones = 1: ones

nats :: [Integer]
--nats = iterate (+1) 0
nats = 0: map (+1) nats


ints :: [Integer]
{-
ints = iterate (ints2 ) 0
    where 
        ints2 :: Integer -> Integer
        ints2 x
            | x > 0 = (-x)
            | otherwise = (-x) + 1

-}
ints = 0 : concat [[x,-x] | x <- tail nats ]




triangulars :: [Integer]
{-
triangulars = tr 0 0
    where
        tr :: Integer -> Integer -> [Integer]
        tr x i = (x+i):(tr (x+i) (i+1))
-}
triangulars = scanl (+) 0 (tail nats) 
-- 0

factorials :: [Integer]
{-
factorials = 0:fact 1 1
    where
        fact :: Integer -> Integer-> [Integer]
        fact n i = (n*i):(fact (n*i) (i+1))
-}
factorials = scanl (*) 1 (tail nats)

fibs :: [Integer]
fibs = 0:1:zipWith (+) (fibs) (tail fibs)



primes :: [Integer]
primes = garbell (drop 2 nats) --{2..}
    where
        garbell :: [Integer] -> [Integer]
        garbell (p:ps) = p : garbell [x | x <- ps , mod x p /= 0]

hammings :: [Integer]
hammings = 1 : merge (map (*2) hammings) (merge (map (*3) hammings ) (map (*5) hammings ))
    where 
        merge :: [Integer] -> [Integer] -> [Integer]
        -- no hace falta casos base xq son  2 listas infinitas
        merge (x:xs) (y:ys)
            | x < y = x : merge xs (y:ys)
            | y < x = y : merge (x:xs) ys
            | otherwise = x : merge xs ys


lookNsay :: [Integer]
lookNsay = iterate count 1

count :: Integer -> Integer
count a = read $ next $ show a

next :: [Char] -> [Char]
next [] = []
next cs = (show n) ++ [pr] ++ next cua
  where 
    pr = head cs
    n = length $ takeWhile ( == pr) cs
    cua = dropWhile ( == pr) cs

tartaglia :: [[Integer]]
tartaglia = iterate (\x-> zipWith (+) (0:x) (x++[0])) [1]