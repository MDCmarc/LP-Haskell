myFoldl :: (a->b->a)->a->[b]->a
myFoldl f a [] = a
myFoldl f a (l:list) = myFoldl f (f a l) list 

myFoldr :: (a->b->b)->b->[a]->b
myFoldr f b [] = b
myFoldr f b (l:list) = f l (myFoldr f b list) 

myIterate :: (a->a)->a->[a]
myIterate f a = a : myIterate f(f a)

myUntil :: (a->Bool)->(a->a)->a->a
myUntil cond f a 
    | cond a  = a
    | otherwise = myUntil cond f $ f a

---Recursion    

myMap :: (a->b)->[a]->[b]
{-
myMap f [] = []
myMap f (l:list) = f l : myMap f list
-}
--myMap f list = [f a | a<- list]
myMap f list = (\x y -> x ++ [f list] ) [] list

myFilter :: (a->Bool)->[a]->[a]
{-
myFilter f [] = []
myFilter f (a:as)
    | f a = a: myFilter f as
    | otherwise = myFilter f as
-}
myFilter f list = [a | a <- list , f a]

myAll :: (a->Bool)->[a]->Bool
{-
myAll f [] = True
myAll f (a:as)
    | f a = myAll f as
    | otherwise = False
-}
--myAll f list = foldl (\x y -> x && f y) True list
myAll f list = and (map f list)

myAny :: (a->Bool)->[a]->Bool
--myAny f list = foldl (\x y -> x||f y) False list
myAny f list = or (map f list)

myZip :: [a]->[b]-> [(a,b)]
myZip [] _ = []
myZip _ []  =[]
myZip (a:as) (b:bs) = (a,b): myZip as bs

myZipWith ::(a->b->c)->[a]->[b]->[c]
myZipWith f a b = [f a b | (a,b) <- myZip a b ]
