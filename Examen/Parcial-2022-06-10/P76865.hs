data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)  

instance Foldable Tree where
 --   foldr:: (a -> b -> b) -> b -> Tree a -> b
    foldr _ b Empty = b 
    foldr f b (Node a fe fd) = f a $ foldr f (foldr f b fd) fe 

--foldl:: (b -> a -> b) -> b -> Tree a -> b
--foldl _ b Empty = b
--foldl f b (Node a fe fd) = foldl f (foldl f ( f b a)  fe) fd

{-
myFoldl f a [] = a
myFoldl f a (l:list) = myFoldl f (f a l) list 

myFoldr f b [] = b
myFoldr f b (l:list) = f l (myFoldr f b list)
-}

avg:: Tree Int -> Double
avg t = fromIntegral(sum t) / (fromIntegral $ length t)

cat :: Tree String -> String
cat t = foldr (\x y ->x++" "++ y) "" t