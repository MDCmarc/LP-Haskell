insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (l:ls) x 
    | x <= l = x:l:ls
    | otherwise = l:(insert ls x)

isort :: [Int] -> [Int]
{-
isort [] = []
isort (l:ls) = insert (isort ls) l
-}
-- se aplica el insert del primer elemento siempre sobre la cola
isort list = foldl insert [] list


remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (l:ls) x
    | l == x = ls
    |otherwise = l:(remove ls x)

ssort :: [Int] -> [Int]
ssort [] = []
-- quito el primer elemento de la listaa y lo inserto ordenado en la lista que ya esta ordenada
ssort (l:ls) = insert  (ssort (remove (l:ls) l) ) l


merge :: [Int] -> [Int] -> [Int]
merge a  [] = a
merge []  b =  b
merge (a:as) (b:bs) 
    | a < b = a: merge as (b:bs)
    |otherwise = b: merge (a:as) bs

msort :: [Int] -> [Int]
msort [] = []
msort [a] = [a]
msort list = merge (msort left) (msort right)
    where
        left = take  halve list
        right = drop halve list
        halve = length list `div` 2

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort $ filter (<x) xs ) ++ [x] ++ (qsort $ filter (>=x) xs)

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:xs) = (genQsort $ filter (<x) xs ) ++ [x] ++ (genQsort $ filter (>=x) xs)