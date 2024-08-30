-- permutations :: [a] -> [[a]]
-- permutations [] = [[]]
-- permutations xs = map (\x -> map (x:) (permutations (removeByElem x xs))) xs [to fix]

permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
    x <- xs 
    perm <- permutations (removeByElem x xs)
    return (x: perm)

removeByElem :: (Eq a) => a -> [a] -> [a]
removeByElem _ [] = []
removeByElem x (y:ys) 
    | (x == y)  = ys
    | otherwise = y:(removeByElem x ys)

removeById :: Int -> [a] -> [a]
removeById id xs = 
    let (lhs, x: rhs) = splitAt id xs
    in lhs ++ rhs
