-- 1
data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

-- 2

-- 3
--  a
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f (NodeT x t1 t2)

countT :: (a -> Bool) -> Tree a -> Int

countLeaves :: Tree a -> Int
countLeaves EmptyT = 1
countLeaves (NodeT _ t1 t2) = countLeaves t1 + countLeaves t2

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

inOrder :: Tree a -> [a]

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : zipConcat (listPerLevel t1) (listPerLevel t2)

zipConcat :: [[a]] -> [[a]] -> [[a]]
zipConcat [] ys = ys
zipConcat xs [] = xs
zipConcat (x:xs) (y:ys) = (x ++ y) : zipConcat xs ys

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT x _ _) = [x]
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = x : laDeMayorLongitud (ramaMasLarga t1) (ramaMasLarga t2)

laDeMayorLongitud :: [a] -> [a] -> [a]
laDeMayorLongitud xs ys = if longitud xs > longitud ys
                           then xs
                           else ys

todosLosCaminos :: Tree a -> [[a]]

-- 4
-- 5

-- a1
-- a2