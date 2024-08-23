data Tree a = Empty | Node a (Tree a) (Tree a)

-- a)
instance Show a => Show (Tree a) where
    show Empty = "()"
    show (Node a fe fd) = "(" ++ show fe ++ "," ++ show a ++ "," ++ show fd ++ ")"


--b)
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node a fe fd) = Node(f a) (fmap f fe) (fmap f fd)

doubleT :: Num a => Tree a -> Tree a
doubleT = fmap (*2)

--c)

data Forest a = Forest [Tree a]
    deriving (Show)

instance Functor Forest where
    fmap f (Forest trees) = Forest $ map (fmap f) trees

doubleF :: Num a => Forest a -> Forest a
doubleF = fmap (*2)