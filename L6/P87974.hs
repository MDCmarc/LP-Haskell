main::IO()
main = do
    c1 <- getLine
    if head c1 == 'A' || head c1 == 'a'
    then  putStrLn "Hello!"
    else putStrLn "Bye!"
