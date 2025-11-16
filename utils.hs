-- slice 25
-- animacion 25
-- spaceship 
-- multiset
-- nim
-- midi

-- funnel
-- json
-- multisetlight
-- parcialito
-- three
-- svson
-- practica sincronica
-- mapa
-- tcommand

-- tortuga NO
-- prefix NO



caminoMasCortoHasta e = foldTree cb cr e
    where cb z _ EmptyT = Nothing
        cr e z f n r1 r2 = if n==e
            then Just []
            else elMasCorto (appendToMaybe Izq (caminoMasCortoHasta x t1))
                 elMasCorto (appendToMaybe Der (caminoMasCortoHasta x t2))

appendToMaybe :: Dir -> Maybe [Dir] -> Maybe [Dir]
appendToMaybe x Nothing = Nothing
appendToMaybe x (Just ys) = Just x:ys

elMasCorto :: Maybe [Dir] -> Maybe [Dir] -> Maybe [Dir]
elMasCorto Nothing m = m
elMasCorto m Nothing = m
elMasCorto (Just xs) (Just ys) = length xs > length ys then Just ys else Just xs

estaEnElCamino :: [Dir] -> Tree a -> Bool
estaEnElCamino xs EmptyT = null xs
estaEnElCamino xs (NodeT n t1 t2) = case xs of
    [] -> True
    (Izq:ys) -> estaEnElCamino ys t1 
    (Der:ys) -> estaEnElCamino ys t2

estaEnElCamino' :: [Dir] -> Tree a -> Bool
estaEnElCamino' = flip (foldT
    (\xs -> null xs) -- null
    (\n r1 r2 xs -> case xs of
            [] -> True
            (Izq:ys) -> r1 ys
            (Der:ys) -> r2 ys))

todosLosCaminosDeDirs :: Tree a -> [[Dir]]
todosLosCaminosDeDirs EmptyT = [[]]
todosLosCaminosDeDirs (NodeT n t1 t2) = map (Izq :) (todosLosCaminosDeDirs t1) ++ map (Der :) (todosLosCaminosDeDirs t1)

-- 1
data Dir = Izq | Der
caminoMasCortoHasta :: Eq a => a -> Tree a -> Maybe [Dir]
estaEnElCamino :: [Dir] -> Tree a -> Bool
todosLosCaminosDeDirs :: Tree a -> [[Dir]]
todosLosCaminosDeDirsHasta :: Eq a => a -> Tree a -> [[Dir]]

-- 2
countT :: Eq a => (a -> Bool) -> Tree a -> Int

para todo e 
   countT (e ==) = length (todosLosCaminosDeDirsHasta e)

-- 3
-- Completar
-- 1) las de listas todas
-- 2) las de árboles binarios
-- 3) unzip :: [(a,b)] -> ([a],[b])
-- 4) partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
-- 5) Representaciones numéricas, por lo menos N y NBin

-- 4
para todo f, t1 y t2.
    heighT (zipWithT f t1 t2) = min (heighT t1) (heighT t2)

Para todo b1, b2.
	not (b1 && b2) = not b1 || not b2

todosLosCaminos :: Tree a -> [[a]]
render :: Image -> Int -> Image