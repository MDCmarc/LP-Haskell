myMap :: (a->b) -> [a] -> [b]
myMap f list = [ f x | x <- list]

myFilter :: (a->Bool) -> [a] -> [a]
myFilter f list = [x | x<-list , f x]

myZipWith :: (a->b->c)->[a]->[b]->[c]
myZipWith f as bs  = [f a b | (a,b)<- zip as bs]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify as bs = [(a,b) | a<- as, b<-bs , mod a b == 0]

factors :: Int -> [Int] 
factors x = [d | d<-[1..x], mod x d == 0]