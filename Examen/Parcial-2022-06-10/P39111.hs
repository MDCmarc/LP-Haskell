import Data.List

type Pos = (Int, Int)  

dins:: Pos -> Bool
dins (x,y) = x >=1 && x<=8 && y >=1 && y<=8 

moviments :: Pos -> [Pos]
moviments (x,y) = filter dins [(x+dx,y+dy)| (dx,dy)<- [(1,2),(1,-2),(2,1),(2,-1),(-1,2),(-1,-2),(-2,1),(-2,-1)]]

potAnar3 :: Pos -> Pos -> Bool
--potAnar3 inici final = final `elem` ( potAnar $ potAnar $ moviments inici)
potAnar3 inici final = final `elem` (concatMap moviments  $ concatMap moviments $ moviments inici)


potAnar :: [Pos] -> [Pos]
potAnar [] = []
potAnar (x:xs) = moviments x ++ potAnar xs


-- con monadas se puede mas facil

potAnar3' :: Pos -> Pos -> Bool
potAnar3' inici final = final `elem` (moviments inici >>= moviments >>= moviments)


{-
-- y con el do
potAnar3' :: Pos -> Pos -> Bool
potAnar3 inici final = do
    p1 <- moviments inci
    p2 <- moviments p1
    p3 <- moviments p3 
    if final `elem` p3 then 

    else []
MAS DIFICIL
-}
-- instance Monad [] where
    -- return x = [x]
    -- list >>= f = concatMap f l