module Golf
    (
    ) where

skips :: [a] -> [[a]]
skips as = map (\i -> map snd . filter (\j -> mod (fst j) i == 0) $ zip [1..] as) [1..(length as)]

localMaxima :: [Integer] -> [Integer]
localMaxima l | length l < 3 = []
localMaxima l@(x:y:z:_) | y > x && y > z= y:(localMaxima $ tail l)
                        | otherwise = localMaxima $ tail l

histogram :: [Integer] -> String
histogram l | maximum l > 9 || minimum l < 0 = "Bad input"
            | otherwise = show $ map (\x -> show x ++ "=" ++ (take (countElement x l) (repeat '*'))) [0..9]

countElement :: Eq a => a -> [a] -> Int
countElement e = length . filter (== e)
