flatten :: [[Int]] -> [Int]
flatten a = foldl (++) [] a

myLength :: String -> Int
myLength a = foldl (\y _ -> y+1 ) 0 a

myReverse :: [Int]-> [Int]
myReverse list = foldr (\x y -> y++[x]) [] list

countIn :: [[Int]] -> Int -> [Int]
countIn list x = map ( \sublist -> length $ filter (==x) sublist) list 

firstWord :: String -> String
firstWord frase = takeWhile (/= ' ') $ dropWhile (== ' ') frase 