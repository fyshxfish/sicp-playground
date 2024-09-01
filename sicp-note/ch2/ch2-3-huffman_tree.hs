type Weight = Int 

data HuffmanTree a = Empty | 
                     Leaf a Weight | 
                     Node (HuffmanTree a) (HuffmanTree a) [a] Weight 
                     deriving (Show, Eq)

data Symbol = A | B | C | D | E | F | G | H deriving (Show, Eq) 

getWeight :: HuffmanTree a -> Weight
getWeight (Leaf _ w) = w 
getWeight (Node _ _ _ w) = w

adjoinTree :: HuffmanTree a  -> [HuffmanTree a] -> [HuffmanTree a]
adjoinTree t [] = [t]
adjoinTree t (t': ts) 
    | w < w'   = t: t': ts 
    | otherwise = t': (adjoinTree t ts)  
    where w  = getWeight t 
          w' = getWeight t' 

moveFirstNode :: [HuffmanTree a] -> [HuffmanTree a]
moveFirstNode (t:ts) = adjoinTree t ts 

initLeafs :: [HuffmanTree a] -> [HuffmanTree a]     -- I know pl(leaf) = leaves, btw. ^^   
initLeafs [] = []   
initLeafs (p:ps) = adjoinTree p (initLeafs ps)

makeNode :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a 
makeNode (Leaf s1 w1) (Leaf s2 w2) = Node (Leaf s1 w1) (Leaf s2 w2) [s1, s2] (w1 + w2)
makeNode (Leaf s w) (Node l r ss w') = Node (Leaf s w) (Node l r ss w') (s:ss) (w + w')  
makeNode (Node l r ss w') (Leaf s w)  = Node (Node l r ss w') (Leaf s w) (ss ++ [s]) (w + w')  
makeNode (Node l1 r1 ss1 w1) (Node l2 r2 ss2 w2) = Node (Node l1 r1 ss1 w1) (Node l2 r2 ss2 w2) (ss1 ++ ss2) (w1 + w2)

constructHuffTree :: [HuffmanTree a] -> HuffmanTree a
constructHuffTree [] = Empty
constructHuffTree [t] = t       -- singleton leaf 
constructHuffTree (x:y:ts) = constructHuffTree $ moveFirstNode $ (makeNode x y): ts

initAndConstructHuffTree :: [HuffmanTree a] -> HuffmanTree a
initAndConstructHuffTree = constructHuffTree . initLeafs 

{- TEST: -}
unorderedLeafs :: [HuffmanTree Symbol]
unorderedLeafs = [(Leaf B 6), (Leaf A 8), (Leaf  C 5), (Leaf E 3), (Leaf D 4), (Leaf  G 1), (Leaf F 2)]
-- SICP example: 
-- unorderedLeafs = [(Leaf A 8), (Leaf B 3), (Leaf  C 1), (Leaf D 1), (Leaf E 1), (Leaf F 1), (Leaf G 1), (Leaf H 1)]

orderedLeafs :: [HuffmanTree Symbol]
orderedLeafs = initLeafs unorderedLeafs