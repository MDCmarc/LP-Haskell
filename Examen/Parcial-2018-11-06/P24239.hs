 --------------------------- ROMAN --------------------------------

roman:: Char -> Int
roman 'I' = 1
roman 'V' = 5
roman 'X' = 10
roman 'L' = 50
roman 'C' = 100
roman 'D' = 500
roman 'M' = 1000

roman2int :: String -> Int
roman2int "" = 0
roman2int [a] = roman a
roman2int (a:b:rest)
    | roman a < roman b = roman2int (b:rest) - roman a
    | otherwise = roman a + roman2int(b:rest)


chose :: Char -> Char -> Int
chose a b 
    | roman a < roman b = - roman a
    | otherwise = roman a 
roman2int' :: String -> Int
roman2int' s = foldl (+) 0 $ zipWith chose s (tail s ++ [last s])


---------------------------------- RAICES ---------------------------------
taylor :: Float -> Float -> Float
taylor x ant = 0.5 * (ant + x/ant)

arrels :: Float -> [Float] 
arrels x =  iterate (taylor x) x

arrel :: Float -> Float -> Float
arrel x error = calculate (arrels x) (tail $ arrels x)
    where 
        calculate :: [Float]-> [Float] -> Float
        calculate ant act
            | head ant - head act < error = head act
            | otherwise = calculate act (tail act)


---------------------------------- ARBOLES ------------------------

data LTree a = Leaf a | Node (LTree a) (LTree a)

instance (Show a) => Show (LTree a) where
    show (Leaf a) = "{" ++ show a ++ "}"
    show (Node treeA treeB) = "<"++ show treeA ++ "," ++ show treeB ++ ">"

build :: [a] -> LTree a
build [node] = Leaf node
build list = Node (build $ take mitad list) (build $ drop mitad list)
    where 
        mitad = (length list + 1) `div` 2


--zipLTrees :: LTree a -> LTree b -> Maybe (LTree(a,b))
-- FALTA