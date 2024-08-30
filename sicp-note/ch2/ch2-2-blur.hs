-- Rewrite it in Haskell!

-- Blur the barrier of data and precedure in Haskell 


rational x y = 
    \s -> s x y

numer r = r (\p q -> p)
denom r = r (\p q -> q)

x = rational 3 4

n = numer x 
d = denom x


