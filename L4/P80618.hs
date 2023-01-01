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

sizeQ :: Queue a -> Int
sizeQ (Queue l1 l2) = (length l1) +  (length l2)

