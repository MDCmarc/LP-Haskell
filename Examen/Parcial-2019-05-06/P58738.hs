data Stree a = Nil | Node Int a (Stree a) (Stree a)
    deriving (Show)

-- a)

isOk :: Stree a -> Bool
isOk Nil = True
isOk (Node t _ fe fd) = isOk fe && isOk fd &&  t-1 ==  (talla fe + talla fd)

talla :: Stree a -> Int
talla Nil = 0
talla (Node t _ _ _) = t

--b)

nthElement :: Stree a -> Int -> Maybe a
nthElement a i 
    |  i > talla a  || i<=0 = Nothing
    | otherwise = Just ((inordre a) !! (i-1))

inordre :: Stree a -> [a]
inordre Nil = []
inordre (Node _ n fe fd) = inordre fe ++ [n] ++ inordre fd

-- c)

mapToElements :: ( a->b ) -> Stree a -> [Int] -> [Maybe b]
mapToElements f s [] = []
mapToElements f s (l:list) = [fmap f (nthElement s l)] ++ mapToElements f s list

instance Functor Stree where
    fmap f Nil = Nil
    fmap f (Node t n fe fd) =  Node t (f n) (fmap f fe) (fmap f fd)