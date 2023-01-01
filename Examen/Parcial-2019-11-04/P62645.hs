
add :: String -> String
add content =  show . sum . map read $ words content

main :: IO()
main = do
    content <- getContents
    putStrLn $ add content
