-- E 2.40
uniquePairs :: Int -> [(Int, Int)]
uniquePairs n = do 
    i <- [1..n]
    j <- [1..(i-1)]
    return (j, i)


-- E 2.41
uniqueTuples :: Int -> [(Int, Int, Int)]
uniqueTuples n = do 
    i <- [1..n]
    j <- [1..(i-1)]
    k <- [1..(j-1)]
    return (k, j, i)

findVeryTuples ::  Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
findVeryTuples s = filter (\(x, y, z) -> (x + y + z) == s)  

genAndFilter :: Int -> Int -> [(Int, Int, Int)]
genAndFilter n s = findVeryTuples s $ uniqueTuples n