fizzBuzz :: [Either Int String]
fizzBuzz = map fizz [0..]
    where 
        fizz :: Int-> Either Int String
        fizz x 
            | mod x 15 == 0 = Right "FizzBuzz"
            | mod x 3 == 0 = Right "Fizz"
            | mod x 5 == 0 = Right "Buzz"
            | otherwise = Left x