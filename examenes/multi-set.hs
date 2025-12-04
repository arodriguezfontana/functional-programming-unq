-- 1. Explicita.
-- a. Describe la cantidad de ocurrencias del elementodado en el multiset.
occursMSE :: a -> MSExp a -> Int
occursMSE x m = occursMSEWith x id m

occursMSEWith :: a -> (a -> a) -> MSExp a -> Int
occursMSEWith x f EmptMS = 0
occursMSEWith x f (AddMS y m) = (if x == f y then 1 else 0) + occursMSEWith x f m
occursMSEWith x f (RemoveMS y m) = if x == f y
    then if occursMSEWith x f m > 0
            then occursMSEWith x f m - 1
            else occursMSEWith x f m
    else occursMSEWith x f m
occursMSEWith x f (UnionMS m1 m2) = occursMSEWith x f m1 + occursMSEWith x f m2
occursMSEWith x f (MapMS g m) = occursMSEWith x (f . g) m

-- b. Describe el multiset resultante de eliminar todas las ocurrencias de los elementos que no cumplen con el predicado dado.
filterMSE :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE p EmptMS = EmptMS
filterMSE p (AddMS x m) = if p x
    then AddMS x (filterMSE p m)
    else filterMSE p m
filterMSE p (RemoveMS x m) = if p x
    then RemoveMS x (filterMSE p m)
    else filterMSE p m
filterMSE p (UnionMS m1 m2) = UnionMS (filterMSE p m1) (filterMSE p m2)
filterMSE p (MapMS f m) = MapMS f (filterMSE (p . f) m)

-- c. Indica si la expresión de multiset dada es válida. Una expresión de multiset es inválida si tiene más RemoveMS que AddMS para un elemento determinado; en el caso de una UnionMS, cada parte se considera por separado.
isValidMSE :: Eq a => MSExp a -> Bool
isValidMSE EmptyMS = True
isValidMSE (AddMS x ms) = isValidMSE ms
isValidMSE (RemoveMS x ms) = occursMSE x ms > 0 && isValidMSE ms
isValidMSE (UnionMS ms1 ms2) = isValidMSE ms1 && isValidMSE ms2
isValidMSE (MapMS g ms) = isValidMSE ms

-- d. Describe la lista de todas las ocurrencias de los elementos del multiset dado.
evalMSE :: Eq a => MSExp a -> [a]
evalMSE EmptMS = []
evalMSE (AddMS x m) = x : evalMSE m
evalMSE (RemoveMS x m) = remove x (evalMSE m)
evalMSE (UnionMS m1 m2) = evalMSE m1 ++ evalMSE m2
evalMSE (MapMS f m) = map f (evalMSE ms)

remove :: Eq a => a -> [a]
remove e [] = []
remove e (x:xs) = if e == x then xs else x : remove e xs

-- e. Suponiendo que recibe una expresión de multiset válida y describe al multiset resultante de simplificar el dado.
simpMSE :: MSExp a -> MSExp a
simpMSE EmptMS = EmptMS
simpMSE (AddMS x m) = AddMS x (simpMSE m)
simpMSE (RemoveMS x m) = simpRemove x (simpMSE m)
simpMSE (UnionMS m1 m2) = simpUnion (simpMSE m1) (simpMSE m2)
simpMSE (MapMS f m) = simpMap f (simpMSE m)

simpUnion :: MSExp a -> MSExp a -> MSExp a
simpUnion EmptMS m = m
simpUnion m EmptMS = m
simpUnion m1 m2 = UnionMS m1 m2

simpMap :: (a->a) -> MSExp a -> MSExp a 
simpMap f EmptMS = EmptMS
simpMap f m = MapMS f m

simpRemove :: Eq a => a -> MSExp a -> MSExp a
simpRemove x (AddMS y m) = if x == y
    then m
    else RemoveMS x (AddMS y m)
simpRemove x m = RemoveMS x m  

-- 2. Demostrar: evalMSE . simpMSE = evalMSE.
por principio de extensionalidad, para todo m:
    ¿(evalMSE . simpMSE) m = evalMSE m?

por definicion de (.), es equivalente a:
    ¿evalMSE (simpMSE m) = evalMSE m?

sea ms::MSExp a, quiero ver que:
    ¿evalMSE (simpMSE ms) = evalMSE ms?

por principio de induccion estructural sobre la estructura de ms, es equivalente a demostrar:
    cb, ms=EmptMS:
        ¿evalMSE (simpMSE EmptMS) = evalMSE EmptMS?
    ci1, ms=(AddMS x ms'):
        hi1: ¡evalMSE (simpMSE ms') = evalMSE ms'!
        ti1: ¿evalMSE (simpMSE (AddMS x ms')) = evalMSE (AddMS x ms')?
    ci2, ms=(RemoveMS x ms'):
        hi2: ¡evalMSE (simpMSE ms') = evalMSE ms'!
        ti2: ¿evalMSE (simpMSE (RemoveMS x ms')) = evalMSE (RemoveMS x ms')?
    ci3, ms=(UnionMS m1 m2):
        hi3.1: ¡evalMSE (simpMSE m1) = evalMSE m1!
        hi3.1: ¡evalMSE (simpMSE m2) = evalMSE m2!
        ti3: ¿evalMSE (simpMSE (UnionMS m1 m2)) = evalMSE (UnionMS m1 m2)?
    ci4, ms=(MapMS f ms'):
        hi4: ¡evalMSE (simpMSE ms') = evalMSE ms'!
        ti4: ¿evalMSE (simpMSE (MapMS f m)) = evalMSE (MapMS f m)?

cbi:
evalMSE (simpMSE EmptMS)
-- simpMSE.1
evalMSE EmptMS

cbd:
evalMSE EmptMS

cb demostrado.

ci1i:
evalMSE (simpMSE (AddMS x ms'))
-- simpMSE.2
evalMSE (AddMS x (simpMSE ms'))
-- evalMSE.2
x : evalMSE (simpMSE ms')
-- hi1
x : evalMSE ms'
-- evalMSE.2
evalMSE (AddMS x ms')

ci1d:
evalMSE (AddMS x ms')

ci1 demostrado.

ci2i:
evalMSE (simpMSE (RemoveMS x ms'))
-- simpMSE.3
evalMSE (simpRemove x (simpMSE ms'))
-- lema.1
remove x (evalMSE (simpMSE ms'))
-- h2
remove x (evalMSE ms')
-- evalMSE.3
evalMSE (RemoveMS x ms')

ci2d:
evalMSE (RemoveMS x ms')

ci2 demostrado.

ci3i:
evalMSE (simpMSE (UnionMS m1 m2))
-- simpMSE.4
evalMSE (simpUnion (simpMSE m1) (simpMSE m2))
-- lema.2
evalMSE (simpMSE m1) ++ evalMSE (simpMSE m2)
-- hi3.1 y hi3.2
evalMSE m1 ++ evalMSE m2
-- evalMSE.4
evalMSE (UnionMS m1 m2)

ci3d:
evalMSE (UnionMS m1 m2)

ci3 demostrado.

ci4i:
evalMSE (simpMSE (MapMS f ms'))
-- simpMSE.5
evalMSE (simpMap f (simpMSE ms'))
-- lema.3
map f (evalMSE (simpMSE ms'))
-- hi4
map f (evalMSE ms)
-- evalMSE.5
evalMSE (MapMS f ms)

ci4d:
evalMSE (MapMS f ms')

ci4 demostrado.

-- lema.1
para todo x. para todo ms:
    ¿evalMSE (simpRemove x ms) = remove x (evalMSE ms)?

sea ms'::MSExp a, y::a, quiero ver que:

por analisis de casos sobre ms':
    c1, ms'=(AddMS x ms)
        ¿evalMSE (simpRemove y (AddMS x ms)) = remove y (evalMSE (AddMS x ms))?
    c2, ms'/=(AddMS x ms)
        ¿evalMSE (simpRemove y ms') = remove y (evalMSE ms')?

c1i:
evalMSE (simpRemove y (AddMS x ms))
-- simpRemove.1
evalMSE (if y == x then ms else RemoveMS x (AddMS y ms))

c1d:
remove y (evalMSE (AddMS x ms))
-- evalMSE.2
remove y (x : evalMSE ms)
-- remove.2
if y == x then evalMSE ms else x : remove y (evalMSE ms)

por particion en subcasos sobre el if:
    sc1, y=x
    sc2, y/=x

sc1-c1i:
evalMSE ms

sc1-c1d:
evalMSE ms

sc1 demostrado.

sc2-c1i:
evalMSE (RemoveMS y (AddMS x ms)))
-- evalMSE.3
remove y (evalMSE (Add x ms))
-- evalMSE.2
remove y (x : evalMSE ms)
-- remove.2
if y == x then evalMSE ms else x : remove y (evalMSE ms) 

sc2-c1d:
if y == x then evalMSE ms else x : remove y (evalMSE ms)

sc2 demostrado.
c1 demostrado.

c2i:
evalMSE (simpRemove y ms')
-- simpRemove.1
evalMSE (RemoveMS y ms')
-- evalMSE.3
remove y (evalMSE ms')

c2d:
remove y (evalMSE ms')

c2 demostrado.

-- lema.2
para todo m1. para todo m2:
    ¿evalMSE (simpUnion m1 m2) = evalMSE m1 ++ evalMSE m2?

sea m1'::MSExp a, m2'::MSExp a, quiero ver que:
    ¿evalMSE (simpUnion m1' m2') = evalMSE m1' ++ evalMSE m2'?

por analisis de casos sobre m1' y m2':
    c1, m1'=EmptMS, m2'=culquier caso
        ¿evalMSE (simpUnion EmptMS m2') = evalMSE EmptMS ++ evalMSE m2'?
    c2, m1'/=EmptMS, m2'=EmptMS
        ¿evalMSE (simpUnion m1' EmptMS) = evalMSE m1' ++ evalMSE EmptMS?
    c3, m1'/=EmptMS, m2'/=EmptMS
        ¿evalMSE (simpUnion m1' m2') = evalMSE m1' ++ evalMSE m2'?

c1i:
evalMSE (simpUnion EmptMS m2')
-- simpUnion.1
evalMSE m2'

c1d:
evalMSE EmptMS ++ evalMSE m2'
-- evalMSE.1
[] ++ evalMSE m2'
-- ++.1
evalMSE m2'

c1 demostrado.

c2i:
evalMSE (simpUnion m1' EmptMS)
-- simpUnion.2
evalMSE m1'

c2d:
evalMSE m1' ++ evalMSE EmptMS
-- evalMSE.1
evalMSE m1' ++ []
-- ++.2
evalMSE m1'

c2 demostrado.

c3i:
evalMSE (simpUnion m1' m2')
-- simpUnion.3
evalMSE (UnionMS m1' m2')
-- evalMSE.4
evalMSE m1' ++ evalMSE m2'

c3d:
evalMSE m1' ++ evalMSE m2'

c3 demostrado.

-- lema.3
para todo f. para todo m:
    ¿evalMSE (simpMap f m) = map f (evalMSE m)?

sea m'::MSExp a, f'::a->a, quiero ver que:
    ¿evalMSE (simpMap f' m') = map f' (evalMSE m')?

por analisis de casos sobre m':
    c1, m'=EmptyMS:
        ¿evalMSE (simpMap f' EmptyMS) = map f' (evalMSE EmptyMS)?
    c2, m'/=EmptyMS:
        ¿evalMSE (simpMap f' m') = map f' (evalMSE m')?

c1i:
evalMSE (simpMap f' EmptyMS)
-- simpMap.1
evalMSE EmptMS
-- evalMSE.1
[]

c1d:
map f' (evalMSE EmptyMS)
-- evalMSE.1
map f' []
-- map.1
[]

c1 demostrado.

c2i:
evalMSE (simpMap f' m')
-- simpMap.2
evalMSE (MapMS f' m')
-- evalMSE.5
map f' (evalMSE m')

c2d:
map f' (evalMSE m')

c2 demostrado.

-- 3. Esquema recursivo y primitivo de MSExp a.
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

-- 4. Con esquemas.
occursMSE' :: a -> MSExp a -> Int
occursMSE' x m = occursMESWith' x id m

occursMSEWith' :: a -> (a -> a) -> MSExp a -> Int
occursMSEWith' x f = recMSE
    0 
    (\y m n -> if x == f y then 1 + n else n)
    (\y m n -> if x == f y then if n > 0 then n - 1 else n else n)
    (\n m nr) -> n + nr -- +
    (\g m n -> occursMSEWith' x (f . g) m)

filterMSE' :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE' p = recMSE 
    EmptyMS
    (\x m mr -> if p x then AddMS x mr else mr)
    (\x m mr -> if p x then RemoveMS x mr else mr)
    (\m1 m2 mr1 mr2 -> Union mr1 mr2)
    (\f m mr -> MapMS f (filterMSE' (p . f) mr))

isValidMSE' :: MSEdp a -> Bool
isValidMSE' = recMSE
    True
    (\x m b -> b) 
    (\x m b -> occursMSE x m > 0 && b) 
    (\m1 m2 b1 b2 -> b1 && b2) 
    (\f b -> b) 

evalMSE' :: Eq a => MSExp a -> [a]
evalMSE' = foldMSE
    []
    (\x rs -> x : rs) -- :
    (\x rs -> remove x rs) -- remove
    (\rs1 rs2 -> rs1 ++ rs2) -- ++
    (\f rs -> map f rs) -- map

simpMSE' :: MSExp a -> MSExp a
simpMSE' = foldMSE
    EmptMS
    (\x m -> AddMS x m) -- AddMS
    (\x m -> simpRemove x m) -- simpRemove
    (\m1 m2 -> simpUnion m1 m2) -- simpUnion
    (\f m -> simpMap f m) -- simpMap

remove' :: Eq a => a -> [a]
remove' e = recr
    (\x xs rs -> if e == x then xs else x : rs)
    []