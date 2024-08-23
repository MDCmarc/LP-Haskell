data Tree a = Node a (Tree a) (Tree a) | Empty 
    deriving (Show)

size :: Tree a -> Int
size Empty  = 0
size (Node _ a b) = 1 + size a + size b

height :: Tree a -> Int
height Empty = 0
height (Node _ a b) = 1 + max (height a)  (height b)

equal :: Eq  a=> Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node _ _ _) = False
equal (Node _ _ _ ) Empty = False
equal (Node a b c) (Node a1 b1 c1 ) =  a == a1 && equal b b1 && equal c c1

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty (Node _ _ _) = False
isomorphic (Node _ _ _ ) Empty = False
isomorphic (Node a b c ) (Node a1 b1 c1) = 
    a == a1 && ((equal b b1 && equal c c1) || equal b c1 && equal c b1)

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a b c) = [a] ++ (preOrder b) ++ (preOrder c)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a b c) =  (postOrder b) ++ (postOrder c) ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a b c) =  (inOrder b) ++ [a] ++ (inOrder c)


breadthFirst :: Tree a -> [a]
breadthFirst x = breadthFirstRec [x]

breadthFirstRec :: [Tree a] -> [a]
breadthFirstRec [] = []
breadthFirstRec (Empty:ts) = breadthFirstRec ts
breadthFirstRec ((Node x left right):ts) = x : (breadthFirstRec $ ts ++ [left, right])



build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build ( x:xs) y = Node x (build preE inE) (build preD inD)
    where
        preE = take (length inE) xs
        preD = drop ( length inD) xs
        inE = takeWhile (/= x) y            -- Cojo el tamaño hasta la raiz del preordn (no hay nodos repetidos)
        inD = tail (dropWhile(/=x) y)          -- Cojo el tamaño desde la raiz del preorden


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ x Empty = x
overlap _ Empty y = y
overlap f (Node x fex fdx) (Node y fey fdy) = Node (f x y) (overlap f fex fey) (overlap f fdx fdy)
    