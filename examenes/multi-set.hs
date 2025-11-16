data MSExp a = EmptMS | AddMS a (MSExp a) | RemoveMS a (MSExp a) | UnionMS (MSExp a) (MSExp a) | MapMS (a->a) (MSExp a)

-- 1. Recursión explícita.
occursMSE :: a -> MSExp a -> Int
occursMSE x m = occursMESWith x id m

occursMESWith :: a -> (a->a) -> MSExp a -> Int
occursMESWith x f EmptMS = 0
occursMESWith x f (AddMS y m) = (if f y == x
    then 1 else 0) + occursMESWith x g m
occursMESWith x f (RemoveMS y m) = if f y == x
    then max 0 (occursMESWith x f m)
    else occursMESWith x f m
occursMESWith x f (UnionMS m1 m2) = occursMESWith x f m1 + occursMESWith x f m2
occursMESWith x f (MapMS g m) = occursMESWith x (f . g) m

filterMSE :: (a->Bool) -> MSExp a -> MSExp a
filterMSE _ EmptMS = EmptMS
filterMSE p (AddMS x m) = if p x
    then AddMS x (filterMSE p m)
    else filterMSE p m
filterMSE p (RemoveMS x m) = if p x
    then RemoveMS x (filterMSE p m)
    else filterMSE p m
filterMSE p (UnionMS m1 m2) = UnionMS (filterMSE p m1) (filterMSE p m2)
filterMSE p (MapMS f m) = MapMS f (filterMSE (p . f) m)

isValidMSE :: MSExp a -> Bool
isValidMSE m = isValidMSEWith id m

isValidMSEWith :: Eq a => (a -> a) -> MSExp a -> Bool
isValidMSEWith f EmptMS = True
isValidMSEWith f (AddMS x m) = isValidMSEWith f m
isValidMSEWith f (RemoveMS x m) = occursMSE x m > 0 && isValidMSEWith f m
isValidMSEWith f (UnionMS m1 m2) = isValidMSEWith m1 && isValidMSEWith m2
isValidMSEWith f (MapMS g m) = isValidMSEWith (f . g) m

evalMSE :: Eq a => MSExp a -> [a]
evalMSE EmptMS = []
evalMSE (AddMS x m) = x : evalMSE m
evalMSE (RemoveMS x m) = remove x (evalMSE m)
evalMSE (UnionMS m1 m2) = evalMSE m1 ++ evalMSE m2
evalMSE (MapMS f m) = map f (evalMSE ms)

remove :: Eq a => a -> [a]
remove _ [] = []
remove e (x:xs) = if e == x then xs else x : remove e xs

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

-- 2. Demostrar: evalMSE . simpMSE = evalMSE
por principio de extensionalidad, para todo m:
    ¿(evalMSE . simpMSE) m = evalMSE m?

por definicion de (.), es equivalente a:
    ¿evalMSE (simpMSE m) = evalMSE m?

sea ms::MSExp, quiero ver que:
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

cb-izq:
evalMSE (simpMSE EmptMS)
-- simpMSE.1
evalMSE EmptMS

cb-der:
evalMSE EmptMS

cb demostrado.

ci1-izq:
evalMSE (simpMSE (AddMS x ms'))
-- simpMSE.2
evalMSE (AddMS x (simpMSE ms'))
-- evalMSE.2
x : evalMSE (simpMSE ms')
-- hi1
x : evalMSE ms'
-- evalMSE.2
evalMSE (AddMS x ms')

ci1-der:
evalMSE (AddMS x ms')

ci1 demostrado.

ci2-izq:
evalMSE (simpMSE (RemoveMS x ms'))
-- simpMSE.3
evalMSE (simpRemove x (simpMSE ms'))
-- lema.1
remove x (evalMSE (simpMSE ms'))
-- h3
remove x (evalMSE ms')
-- evalMSE.3
evalMSE (RemoveMS x ms')

ci2-der:
evalMSE (RemoveMS x ms')

ci2 demostrado.

ci3-izq:
evalMSE (simpMSE (UnionMS m1 m2))
-- simpMSE
evalMSE (simpUnion (simpMSE m1) (simpMSE m2))
-- lema.2
evalMSE (simpMSE m1) ++ evalMSE (simpMSE m2)
-- hi3.1 y hi3.2
evalMSE m1 ++ evalMSE m2
-- evalMSE.4
evalMSE (UnionMS m1 m2)

ci3-der:
evalMSE (UnionMS m1 m2)

ci3 demostrado.

-- ci4


-- lema.1
para todo x. para todo ms:
    ¿evalMSE (simpRemove x ms) = remove x (evalMSE ms)?

sea ms'::MSExp a, y::a, quiero ver que:

por analisis de casos sobre ms':
    c1, ms'=(AddMS x ms)
        ¿evalMSE (simpRemove y (AddMS x ms)) = remove y (evalMSE (AddMS x ms))?
    c2, ms'/=(AddMS x ms)
        ¿evalMSE (simpRemove y ms') = remove y (evalMSE ms')?

c1-izq:
evalMSE (simpRemove y (AddMS x ms))
-- simpRemove.1
evalMSE (if y == x then ms else RemoveMS x (AddMS y ms))

c1-der:
remove y (evalMSE (AddMS x ms))
-- evalMSE.2
remove y (x : evalMSE ms)
-- remove.2
if y == x then evalMSE ms else x : remove y (evalMSE ms)

particion en subcasos sobre el if:
    sc1, y=x
    sc2, y/=x

sc1-c1-izq:
evalMSE ms

sc1-c1-der:
evalMSE ms

sc1 demostrado.

sc2-c1-izq:
evalMSE (RemoveMS y (AddMS x ms)))
-- evalMSE.3
remove y (evalMSE (Add x ms))
-- evalMSE.2
remove y (x : evalMSE ms)
-- remove.2
if y == x then evalMSE ms else x : remove y (evalMSE ms) 

sc2-c1-der:
if y == x then evalMSE ms else x : remove y (evalMSE ms)

sc2 demostrado.
c1 demostrado.

c2-izq:
evalMSE (simpRemove y ms')
-- simpRemove.1
evalMSE (RemoveMS y ms')
-- evalMSE.3
remove y (evalMSE ms')

c2-der:
remove y (evalMSE ms')

c2 demostrado.

-- lema.2
para todo m1. para todo m2:
    ¿evalMSE (simpUnion m1 m2) = evalMSE m1 ++ evalMSE m2?

sea
m1'::MSExp a
m2'::MSExp a
quiero ver que:
    ¿evalMSE (simpUnion m1' m2') = evalMSE m1' ++ evalMSE m2'?

por analisis de casos sobre m1' y m2':
    c1, m1=EmptMS, m2=culquier caso
        ¿evalMSE (simpUnion EmptMS m2') = evalMSE EmptMS ++ evalMSE m2'?
    c2, m1/=EmptMS, m2=EmptMS
        ¿evalMSE (simpUnion m1' EmptMS) = evalMSE m1' ++ evalMSE EmptMS?
    c3, m1/=EmptMS, m2/=EmptMS
        ¿evalMSE (simpUnion m1' m2') = evalMSE m1' ++ evalMSE m2'?

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

-- 4. Sin recursión explicita.
occursMSE :: a -> MSExp a -> Int
occursMES = 

filterMSE :: (a->Bool) -> MSExp a -> MSExp a

isValidMSE :: MSEdp a -> Bool

evalMSE :: Eq a => MSExp a -> [a]

simpMSE :: MSExp a -> MSExp a