{- data abstraction -}

type Weight = Int 

data HuffmanTree a = Empty | 
                     Leaf a Weight | 
                     Node (HuffmanTree a) (HuffmanTree a) [a] Weight 
                     deriving Show

data Symbol = A | B | C | D | E | F | G | H deriving Show

{- Huffman Tree Construction / encode -}

getWeight :: HuffmanTree a -> Weight
getWeight (Leaf _ w) = w 
getWeight (Node _ _ _ w) = w

adjoinTree :: HuffmanTree a  -> [HuffmanTree a] -> [HuffmanTree a]
adjoinTree t [] = [t]
adjoinTree t (t':ts) 
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

{-  another form of code -}

data Bit = L | R deriving Show 

type Bits = [Bit] 

getCode' :: HuffmanTree a -> Bits -> [(a, Bits)]
getCode' (Node (Leaf s1 _) (Leaf s2 _) _ _) rec = [(s1, rec ++ [L]), (s2, rec ++ [R])]
getCode' (Node (Leaf s' _) node _ _) rec = [(s', rec ++ [L])] ++ getCode' node (rec++[R]) 
getCode' (Node node (Leaf s' _) _ _) rec =  getCode' node (rec++[L]) ++ [(s', rec ++ [R])]
getCode' (Node nodel noder  _ _) rec = getCode' nodel (rec++[L]) ++ getCode' noder (rec++[R])

-- Huffman 树的构造步是：合并两个结点，得到父结点，所以不存在 Node 的一个子树为 Empty 的情况
-- 递归基是 Node 中含 Leaf 的情况，检查到 Node 中含 Leaf，不会对 Leaf 进行继续递归，所以不用写 getCode' (Leaf s _)

getCode :: HuffmanTree a -> [(a, Bits)]
getCode t  = getCode' t []


{- decode -}

decode' :: HuffmanTree a -> HuffmanTree a -> Bits -> [a]
decode' originT (Node (Leaf s _) _ _ _) (L:bs) = s: (decode' originT originT bs)

-- decode one symbol
decodeOne :: HuffmanTree a -> Bits -> (a, Bits)
decodeOne (Node (Leaf s _) _ _ _) (L:bs) = (s, bs)
decodeOne (Node _ (Leaf s _) _ _) (R:bs) = (s, bs)
decodeOne (Node node _ _ _) (L:bs) = decodeOne node bs
decodeOne (Node _ node _ _) (R:bs) = decodeOne node bs

-- decode from root 
decode :: HuffmanTree a -> Bits -> [a]
decode _ [] = []
decode t bs = 
    let (s, remainBits) = decodeOne t bs
    in s: decode t remainBits


{- TEST -}
unorderedLeafs :: [HuffmanTree Symbol]
unorderedLeafs = [(Leaf B 6), (Leaf A 8), (Leaf  C 5), (Leaf E 3), (Leaf D 4), (Leaf  G 1), (Leaf F 2)]
-- SICP example: 
-- unorderedLeafs = [(Leaf A 8), (Leaf B 3), (Leaf  C 1), (Leaf D 1), (Leaf E 1), (Leaf F 1), (Leaf G 1), (Leaf H 1)]

orderedLeafs :: [HuffmanTree Symbol]
orderedLeafs = initLeafs unorderedLeafs

huffTree :: HuffmanTree Symbol
huffTree = initAndConstructHuffTree unorderedLeafs

codes :: [(Symbol, Bits)]
codes = getCode huffTree

-- code = [R, L, L, L] -- ~ [A, B]
code :: Bits
code = [L, R, L, R, R, L, L, R, R, L, L, R, L] -- ~ [E, D, G, E]

syms :: [Symbol]
syms = decode huffTree code 