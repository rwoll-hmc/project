module Utils where

-- SOURCE: http://stackoverflow.com/questions/30588221/check-a-string-if-it-contains-a-given-substring-and-return-boolean
substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

-- SOURCE: http://stackoverflow.com/questions/30588221/check-a-string-if-it-contains-a-given-substring-and-return-boolean
prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys
