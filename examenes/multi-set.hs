data MSExp a = EmptMS
    | AddMS a (MSExp a)
    | RemoveMS a (MSExp a)
    | UnionMS (MSExp a) (MSExp a)
    | MapMS (a->a) (MSExp a)

EmptMS
    | AddMS x m
    | RemoveMS x m
    | UnionMS m1 m2
    | MapMS f m

-- 1. Recursión explícita.
-- a
occursMSE :: a -> MSExp a -> Int
occursMSE x m = ???

occursMESWith :: a -> (a->a) -> MSExp a -> Int
occursMESWith _ _ EmptMS = 0
occursMESWith x f (AddMS y m) = unoSi (x==y) + occursMESWith x f m
occursMESWith x f (RemoveMS y m) = if (occursMESWith x f m > 0) then occursMESWith x f m - 1 else occursMESWith x f m
occursMESWith x f (UnionMS m1 m2) = occursMESWith x f m1 + occursMESWith x f m2
occursMESWith x f (MapMS g m) = occursMESWith x f -- ???

-- b
filterMSE :: (a->Bool) -> MSExp a -> MSExp a
filterMSE _ EmptMS = EmptMS
filterMSE p (AddMS x m) = if p x then AddMS x (filterMSE p m) else filterMSE p m
filterMSE p (RemoveMS x m) = if p x then RemoveMS x (filterMSE p m) else filterMSE p m
filterMSE p (UnionMS m1 m2) = UnionMS (filterMSE p m1) (filterMSE p m2)
filterMSE p (MapMS f m) = MapMS f (filterMSE p m) -- ???

-- c
isValidMSE :: MSEdp a -> Bool
isValidMSE EmptMS = True
isValidMSE (AddMS x m) = isValidMSE m
isValidMSE (RemoveMS x m) = occursMSE x m > 0 && isValidMSE m
isValidMSE (UnionMS m1 m2) = isValidMSE m1 && isValidMSE m2
isValidMSE (MapMS f m) = isValidMSE m

-- d
evalMSE :: Eq a => MSExp a -> [a]

-- e
simpMSE :: MSExp a -> MSExp a

-- 2. Demostrar: evalMSE . simpMSE = evalMSE

-- 3. Esquemas de recursión estructural y primitiva para MSExp.
foldMSE :: b -> (a -> b -> b) -> (a -> b -> b) -> (b -> b -> b) -> ((a->a) -> b -> b) -> MSExp a -> b
foldMSE e af rf uf mf x f EmptMS = e
foldMSE e af rf uf mf x f (AddMS x m) = af x (foldMSE e af rf uf mf m)
foldMSE e af rf uf mf x f (RemoveMS x m) = rf x (foldMSE e af rf uf mf m)
foldMSE e af rf uf mf x f (UnionMS x m1 m2) = uf (foldMSE e af rf uf mf m1) (foldMSE e af rf uf mf m2)
foldMSE e af rf uf mf x f (MapMS f m) = mf f (foldMSE e af rf uf mf m)

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