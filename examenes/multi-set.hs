data MSExp a = EmptMS
    | AddMS a (MSExp a)
    | RemoveMS a (MSExp a)
    | UnionMS (MSExp a) (MSExp a)
    | MapMS (a->a) (MSExp a)

-- 1. Recursión explícita.
-- a
occursMSE :: a -> MSExp a -> Int

-- b
filterMSE :: (a->Bool) -> MSExp a -> MSExp a

-- c
isValidMSE :: MSEdp a -> Bool

-- d
evalMSE :: Eq a => MSExp a -> [a]

-- e
simpMSE :: MSExp a -> MSExp a

-- 2. Demostrar: evalMSE . simpMSE = evalMSE

-- 3. Esquemas de recursión estructural y primitiva para MSExp.
foldMSE :: b -> (a -> b -> b) -> (a -> b -> b) -> (b -> b -> b) -> ((a->a) -> b -> b) -> MSExp a -> b
foldMSE e af rf uf mf EmptMS = e
foldMSE e af rf uf mf (AddMS x m) = af x (foldMSE e af rf uf mf m)
foldMSE e af rf uf mf (RemoveMS x m) = rf x (foldMSE e af rf uf mf m)
foldMSE e af rf uf mf (UnionMS x m1 m2) = uf (foldMSE e af rf uf mf m1) (foldMSE e af rf uf mf m2)
foldMSE e af rf uf mf (MapMS f m) = mf f (foldMSE e af rf uf mf m)

recMSE :: b -> (a -> MSExp a -> b -> b) -> (a -> MSExp a -> b -> b) -> (b -> MSExp a -> MSExp a -> b -> b) -> ((a->a) -> MSExp a -> b -> b) -> MSExp a -> b
recMSE e af rf uf mf EmptMS = e
recMSE e af rf uf mf (AddMS x m) = af x m (recMSE e af rf uf mf m)
recMSE e af rf uf mf (RemoveMS x m) = rf x m (recMSE e af rf uf mf m)
recMSE e af rf uf mf (UnionMS x m1 m2) = uf m1 m2 (recMSE e af rf uf mf m1) (recMSE e af rf uf mf m2)
recMSE e af rf uf mf (MapMS f m) = mf f m (recMSE e af rf uf mf m)

-- 4. Sin utilizar recursión explicita.
-- a
occursMSE :: a -> MSExp a -> Int

-- b
filterMSE :: (a->Bool) -> MSExp a -> MSExp a

-- c
isValidMSE :: MSEdp a -> Bool

-- d
evalMSE :: Eq a => MSExp a -> [a]

-- e
simpMSE :: MSExp a -> MSExp a