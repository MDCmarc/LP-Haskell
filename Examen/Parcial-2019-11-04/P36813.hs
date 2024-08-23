import Data.List

endPoint :: Eq a => a -> (a, a) -> Bool
endPoint node (u, v) = node == u || node == v

{-
degree :: Eq a => [(a,a)] -> a -> Int
degree list k = degree2 list k 0
    where 
        degree2 :: Eq a => [(a,a)] -> a -> Int -> Int
        degree2 [] _ r = r
        degree2 ((b,c) : list) k r 
            | b == k || c == k = degree2 list k (r+1)
            | otherwise = degree2 list k r
-}

degree :: Eq a => [(a,a)] -> a -> Int
degree [] x = 0
degree ((a,b):l) x = fromEnum ( a==x || b ==x) + degree l x

degree' :: Eq a => [(a, a)] -> a -> Int
-- degree' list a =  length $ filter (endPoint a) list
degree' l z = lenght ( [(x,y) | (x,y) <- l, x==z || y == z])


neighbors :: Ord a => [(a, a)] -> a -> [a]
--neighbors edges node = sort $ map other $ filter (endPoint node) edges
--    where other (u, v) = if u == node then v else u

neighbors l x = sort (neighbors' l x)
    where 
        neighbors' [] x = []
        neighbors' ((a,b):l) x
            | a == x = b:neighbors' l x
            | b == x = a:neighbors' l x
            | otherwise = neighbors' l x