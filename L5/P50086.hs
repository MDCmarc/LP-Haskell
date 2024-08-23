data Queue a = Queue [a] [a] deriving (Show)
 
create :: Queue a
create = Queue [] []


push :: a -> Queue a -> Queue a
push x (Queue a b) = Queue a (x:b)

pop :: Queue a -> Queue a
pop (Queue [] b ) = Queue (tail $ myReverse b ) []
pop (Queue a b) = Queue (tail a) b

top :: Queue a -> a
top (Queue [] b) = last b
top (Queue a b) = head a

empty :: Queue a -> Bool
empty (Queue a b) = null a && null b

instance Eq a => Eq (Queue a) where
    Queue [] [] == Queue [] [] = True
    q1 == q2
        | empty q1 || empty q2 = False
        | otherwise = (top q1 == top q2) && (pop q1 == pop q2)


myReverse :: [a]-> [a]
myReverse list = foldr (\x y -> y++[x]) [] list

instance Functor Queue
    where 
        fmap f (Queue l1 l2) = (Queue (fmap f l1) ( fmap f l2))


translation :: Num b => b -> Queue b -> Queue b
translation num q = fmap (+ num) q


q2l :: (Queue a) -> [a]
q2l (Queue l1 l2) = (l1 ++ (reverse l2))

instance Applicative Queue  where
    pure x = (Queue [x] [])
    qfn <*> q = (Queue l [])
        where
            l = (q2l qfn) <*> (q2l q)



instance Monad Queue
    where
        return x = (Queue [x] [])
        q >>= fn = (Queue l [])
            where
                l = (q2l q) >>= (q2l . fn)


kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter fn q = do
    x <- q
    if (fn x) then return x else (Queue [] [])