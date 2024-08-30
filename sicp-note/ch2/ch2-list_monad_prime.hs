-- Created by fyshx

-- SICP 2.2.3 Nested Mappings

isDivisible :: Int -> Int -> Bool 
isDivisible x y 
    | mod x y == 0  = True 
    | otherwise     = False 

isPrime :: Int -> Bool
isPrime x 
    | x <= 2    = True 
    | otherwise = not (foldr (||) False (map (isDivisible x) [2..((floor . sqrt . fromIntegral) x)]))


genPairs :: Int -> [((Int, Int), Int)]
genPairs n = do 
    x <- [1..n]
    y <- [1..(x-1)]
    return ((y, x), (x + y))

sumPrimePairs :: Int -> [((Int, Int), Int)]
sumPrimePairs = (filter (\(_, s) -> (isPrime s))) . genPairs




