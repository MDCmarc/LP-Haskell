divisors :: Int -> [Int]
divisors n = [ x | x<-[1..n] , mod n x == 0]

nbDivisors :: Int -> Int
nbDivisors n = foldl (\x _ -> x+1) 0 (divisors n)

moltCompost :: Int -> Bool
moltCompost n = null [x | x<-[1..(n-1)], nbDivisors x >= nbDivisors n]