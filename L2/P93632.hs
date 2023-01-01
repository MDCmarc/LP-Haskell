eql :: [Int] -> [Int] -> Bool
eql a b = midenIgual && elementsIguals
    where 
        midenIgual = length a == length b 
        elementsIguals =  (and $ zipWith (==) a b)

prod :: [Int] -> Int
--prod list = foldl (*) 1 list
-- como esta list a cada lado puedes quitarlo
prod = foldl (*) 1

prodOfEvens :: [Int] -> Int 
--prodOfEvens list = foldl (*) 1 $ filter even list
prodofEvens  = prod.(filter even)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct a b = foldl (+) 0 $ zipWith (*) a b
