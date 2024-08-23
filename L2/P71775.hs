countIf :: (Int -> Bool) -> [Int] -> Int
countIf f a = length $ filter f a 

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam list listf = [map f list | f<- listf]


pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 list listf = [ [ f l | f<- listf] | l<- list]


filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int 
filterFoldl cond f a list = foldl (f) a $filter (cond) list

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f [] a = [a]
insert f (l:list) a
    | f a l = a:l:list
    | otherwise = l:(insert f list a)

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort _ [] = []
insertionSort f list = foldl (insert f) [] list
