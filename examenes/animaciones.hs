data Accion a = Paso a 
    | SaltoArriba a
    | SaltoAdelante a
    | Girar a
type Tiempo = Int
type Duracion = Int
data Animacion a = Espera Duracion
    | Mov Duracion (Accion a)
    | Sec (Animacion a) (Animacion a)
    | Par (Animacion a) (Animacion a)
type Frame a = [Accion a]
type Simulador a = Tiempo -> Frame a

duracion (Espera d) =
duracion (Mov d ac) =
duracion (Sec a1 a2) =
duracion (Par a1 a2) =

-- 1: Debe realizar UN único recorrido sobrecada lista.
combinarSinDuplicados :: [Int] -> [Int] -> [Int]
combinarSinDuplicados [] ys = ys
combinarSinDuplicados xs [] = xs
combinarSinDuplicados (x:xs) (y:ys) = if x == y
    then x : combinarSinDuplicados xs ys
    else if x < y
        then x : combinarSinDuplicados xs (y:ys)
        else y : combinarSinDuplicados (x:xs) ys

-- 2: Recursión explícita.
-- a
duracion :: Animacion a -> Int
duracion (Espera d) = d
duracion (Mov d _) = d
duracion (Sec a1 a2) = duracion a1 + duracion a2
duracion (Par a1 a2) = max (duracion a1) (duracion a2)

-- b
alargar :: Int -> Animacion a -> Animacion a
alargar n (Espera d) = Espera (n*d)
alargar n (Mov d ac) = Mov (n*d) ac
alargar n (Sec a1 a2) = Sec (alargar n a1) (alargar n a2)
alargar n (Par a1 a2) = Par (alargar n a1) (alargar n a2)

-- c
simular :: Animacion a -> [Frame a]
simular (Espera d) = replicate d []
simular (Mov d ac) = replicate d [ac]
simular (Sec a1 a2) = simular a1 ++ simular a2
simular (Par a1 a2) = zipLong (simular a1) (simular a2)

zipLong :: [[a]] -> [[a]] -> [[a]]
zipLong [] yss = yss
zipLong xss [] = xss
zipLong (xs:xss) (ys:yss) = (xs++ys) : zipLong xss yss

-- d
tiemposDeEspera :: Animacion a -> [Tiempo]
tiemposDeEspera (Espera d) = 
tiemposDeEspera (Mov d ac) = 
tiemposDeEspera (Sec a1 a2) = tiemposDeEspera a1 ++ tiemposDeEspera  -- map
tiemposDeEspera (Par a1 a2) = (tiemposDeEspera a1) (tiemposDeEspera a2)

[3,0,5,6,0,0] [7,3,2]

combinarSinDuplicados :: [Int] -> [Int] -> [Int]

[1,2,3,4,5,6,7,8,9,10]
----------------------
[1,2,3,4,5,6]     [7,8,9,10]
[1,2,3,4] 
----------------------
[3,4,5]
[2,3]
----------------------
[3,5]

contarHasta :: Int -> [Int]
contarHasta n = reverse (contarHasta' n)

contarHasta' :: Int -> [Int]
contarHasta' 1 = [1]
contarHasta' n = n : contarHasta' (n-1)

-- 3: Demostrar que para todo k >= 0. duracion . (alargar k) = (k*) . duracion
por ppio. ext. para todo a:
    ¿(duracion . (alargar k)) a = ((k*) . duracion) a?
-- (. y asoc)
    ¿duracion (alargar k a) = k* duracion a?

sea i::Int, an::Animacion a, quiero ver que:
    ¿duracion (alargar i an) = i* duracion an?

por ppio. de ind. en la est. an:
    cb1, an=(Espera d)
        ¿duracion (alargar i (Espera d)) = i* duracion (Espera d)?

    cb2, an=(Mov d ac)
        ¿duracion (alargar i (Mov d ac)) = i* duracion (Mov d ac)?

    ci1, an=(Sec a1 a2)
        hi1: ¡duracion (alargar i a1) = i* duracion a1!
        hi2: ¡duracion (alargar i a2) = i* duracion a2!
        ti: ¿duracion (alargar i (Sec a1 a2)) = i* duracion (Sec a1 a2)?

    ci2, an=(Par a1 a2)
        hi1: ¡duracion (alargar i a1) = i* duracion a1!
        hi2: ¡duracion (alargar i a2) = i* duracion a2!
        ti: ¿duracion (alargar i (Par a1 a2)) = i* duracion (Par a1 a2)?

cb1i:
duracion (alargar i (Espera d))
duracion (Espera (i*d))
i*d

cb1d:
i* duracion (Espera d)
i* d

cb2i:
duracion (alargar i (Mov d ac))
duracion (Mov (i*d) ac)
i*d

cb2d:
i* duracion (Mov d ac)
i* d

ci1i:
duracion (alargar i (Sec a1 a2))
duracion (Sec (alargar i a1) (alargar i a2))
duracion (alargar i a1) + duracion (alargar i a2)
i* duracion a1 + i* duracion a2

ci1d:
i* duracion (Sec a1 a2)
i* (duracion a1 + duracion a2)
i* duracion a1 + i* duracion a2

ci2i:
duracion (alargar i (Par a1 a2))
duracion (Par (alargar i a1) (alargar i a2))
max (duracion (alargar i a1)) (duracion (alargar i a2))
max (i* (duracion a1) (i* (duracion a2)
i* max (duracion a1) (duracion a2)

ci2d:
i* duracion (Par a1 a2)
i* max (duracion a1) (duracion a2)

-- lema
max (i* n) (i* m) = i* max n m

li:
max (i* n) (i* m)
if i* n > i* m then i* n else i* m
-- simplificacion matematica (porque es positivo todo)
if n > m then i* n else i* m
-- if b then f a else f b = f (if b then a else b)
i* (if n > m then n else m)

ld:
i* max n m
i* (if n > m then n else m)

-- 4: Recursion estructural y primitiva para Animacion.
foldA :: (Duracion->b) -> (Duracion->Accion a->b) -> (b->b->b) -> (b->b->b) -> Animacion a -> b
foldA ef mf sf pf (Espera i) = ef i
foldA ef mf sf pf (Mov i ac) = mf i ac
foldA ef mf sf pf (Sec a1 a2) = sf (foldA ef mf sf pf a1) (foldA ef mf sf pf a2)
foldA ef mf sf pf (Par a1 a2) = pf (foldA ef mf sf pf a1) (foldA ef mf sf pf a2)

data Animacion a = Espera Duracion
    | Mov Duracion (Accion a)
    | Sec (Animacion a) (Animacion a)
    | Par (Animacion a) (Animacion a)

-- 5: Ejercicio 2 utilizando esquemas.
simular :: Animacion a -> [Frame a]
simular (Espera d) = replicate d []
simular (Mov d ac) = replicate d [ac]
simular (Sec a1 a2) = simular a1 ++ simular a2
simular (Par a1 a2) = zipLong (simular a1) (simular a2)

simularFoldA :: Animacion a -> [Frame a]
simularFoldA = foldA (\d -> replicate d [])
                (\d ac -> replicate d [ac])
                (\fs1 fs2 -> fs1 ++ fs2)
                (\fs1 fs2 -> zipLong fs1 fs2)

zipLong :: [[a]] -> [[a]] -> [[a]]
zipLong [] yss = yss
zipLong xss [] = xss
zipLong (xs:xss) (ys:yss) = (xs++ys) : zipLong xss yss

zipLongFold :: [[a]] -> ([[a]] -> [[a]])
zipLongFold = foldr (\xs f yss -> case yss of
    [] -> xs : (f yss)
    (ys:yss') -> (xs++ys) : f yss') yss

zipLongRec :: [[a]] -> ([[a]] -> [[a]]) -- tengo que retornar la cola de la recursion
zipLongRec = recr (\yss -> yss) (\xs xss f yss -> case yss of
    [] -> xs:xss
    (ys:yss') -> (xs++ys) : f yss') -- id (solamente)

-- x primer elemento, f resultado, yss lo que me 
-- xss? estoy recorriendo (esta adentro de f)

foldr :: (a -> ([[a]] -> [[a]]) -> [[a]] -> [[a]]) -> ([[a]] -> [[a]]) -> [a] -> ([[a]] -> [[a]])
foldr :: (a -> ([[a]] -> [[a]]) -> ([[a]] -> [[a]])) -> ([[a]] -> [[a]]) -> [a] -> ([[a]] -> [[a]])

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

-- 6: Utilizando esquemas.
-- a
ciclar :: Animacion a -> Simulador a

-- b
combinar :: [Animacion a] -> [Animacion a] -> Animacion a

-- c
mezclar :: [Simulador a] -> Duracion -> [Frame a]