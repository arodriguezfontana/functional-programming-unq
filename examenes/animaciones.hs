-- 1. Explicita.
-- a. Dadas dos listas de números sin repetidos, ordenadas en forma creciente, describe una lista ordenada y sin repetidos que contiene los elementos de ambas listas.
combinarSinDuplicados :: [Int] -> [Int] -> [Int]
combinarSinDuplicados [] ys = ys
combinarSinDuplicados xs [] = xs
combinarSinDuplicados (x:xs) (y:ys) = if x == y
    then x : combinarSinDuplicados xs ys
    else if x < y
        then x : combinarSinDuplicados xs (y:ys)
        else y : combinarSinDuplicados (x:xs) ys

-- 2. Explicita.
-- a. Describe la duración total de la animación (considerando que las acciones en paralelo terminan cuando termina la más larga).
duracion :: Animacion a -> Int
duracion (Espera d) = d
duracion (Mov d ac) = d
duracion (Sec a1 a2) = duracion a1 + duracion a2
duracion (Par a1 a2) = max (duracion a1) (duracion a2)

-- b. Describe una animación donde la duración de cada acción está multiplicada por un factor con respecto a la duración original.
alargar :: Int -> Animacion a -> Animacion a
alargar n (Espera d) = Espera (n*d)
alargar n (Mov d ac) = Mov (n*d) ac
alargar n (Sec a1 a2) = Sec (alargar n a1) (alargar n a2)
alargar n (Par a1 a2) = Par (alargar n a1) (alargar n a2)

-- c. Describe una lista con un frame por cada tiempo de la duración de la animación. Cada frame es una lista con las acciones que ocurren simultáneamente en el tiempo indicado
simular :: Animacion a -> [Frame a]
simular (Espera d) = replicate d []
simular (Mov d ac) = replicate d [ac]
simular (Sec a1 a2) = simular a1 ++ simular a2
simular (Par a1 a2) = zipLong (simular a1) (simular a2)

zipLong :: [[a]] -> [[a]] -> [[a]]
zipLong [] yss = yss
zipLong xss [] = xss
zipLong (xs:xss) (ys:yss) = (xs++ys) : zipLong xss yss

-- d. Describe una lista de los tiempos de la animación donde no hay ninguna acción.
tiemposDeEspera :: Animacion a -> [Tiempo]
tiemposDeEspera (Espera d) = contarHasta d
tiemposDeEspera (Mov d ac) = []
tiemposDeEspera (Sec a1 a2) = tiemposDeEspera a1 ++ map (+ duraccion a1) (tiemposDeEspera a2)
tiemposDeEspera (Par a1 a2) = combinarSinDuplicados (tiemposDeEspera a1) (tiemposDeEspera a2)

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta (n-1) ++ [n]

-- 3. Demostrar: para todo k >= 0. duracion . (alargar k) = (k*) . duracion.
por principio de extensionalidad, para todo k'>=0, para todo a':
    ¿(duracion . (alargar k')) a '= ((k'*) . duracion) a'?

por definicion de (.), es equivalente a:
    ¿duracion (alargar k' a') = k'* duracion a'?

sea k::Int, a::Animacion a,
por principio de induccion en la estructura a, quiero ver que:
    cb1, a=(Espera d)
        ¿duracion (alargar k (Espera d)) = k* duracion (Espera d)?
    cb2, a=(Mov d ac)
        ¿duracion (alargar k (Mov d ac)) = k* duracion (Mov d ac)?
    ci1, an=(Sec a1 a2)
        hi1: ¡duracion (alargar k a1) = k* duracion a1!
        hi2: ¡duracion (alargar k a2) = k* duracion a2!
        ti: ¿duracion (alargar k (Sec a1 a2)) = k* duracion (Sec a1 a2)?
    ci2, an=(Par a1 a2)
        hi1: ¡duracion (alargar k a1) = k* duracion a1!
        hi2: ¡duracion (alargar k a2) = k* duracion a2!
        ti: ¿duracion (alargar k (Par a1 a2)) = k* duracion (Par a1 a2)?

cb1i:
duracion (alargar k (Espera d))
duracion (Espera (k*d))
k*d

cb1d:
k* duracion (Espera d)
k* d

cb2i:
duracion (alargar k (Mov d ac))
duracion (Mov (k*d) ac)
k*d

cb2d:
k* duracion (Mov d ac)
k* d

ci1i:
duracion (alargar k (Sec a1 a2))
duracion (Sec (alargar k a1) (alargar k a2))
duracion (alargar k a1) + duracion (alargar k a2)
k* duracion a1 + k* duracion a2

ci1d:
k* duracion (Sec a1 a2)
k* (duracion a1 + duracion a2)
k* duracion a1 + k* duracion a2

ci2i:
duracion (alargar k (Par a1 a2))
duracion (Par (alargar k a1) (alargar k a2))
max (duracion (alargar k a1)) (duracion (alargar  a2))
max (k* (duracion a1) (k* (duracion a2)
-- lema
k* max (duracion a1) (duracion a2)

ci2d:
k* duracion (Par a1 a2)
k* max (duracion a1) (duracion a2)

-- lema
para todo i. para todo n'. para todo m'.
    ¿max (i* n') (i* m') = i* max n' m'?

sea k::Int, n::Int, m::Int, quiero ver que:
    ¿max (k* n) (k* m) = k* max n m?

li:
max (k* n) (k* m)
-- max
if k* n > k* m then k* n else k* m
-- aritm.
if n > m then k* n else k* m
-- if b then f a else f b = f (if b then a else b)
i* (if n > m then n else m)

ld:
i* max n m
-- max
i* (if n > m then n else m)

-- 4. Esquema recursivo y primitivo de Animacion a.
foldA :: (Duracion -> b) -> (Duracion -> Accion a -> b) -> (b -> b -> b) -> (b -> b ->b) -> Animacion a -> b
foldA ef mf sf pf (Espera i) = ef i
foldA ef mf sf pf (Mov i ac) = mf i ac
foldA ef mf sf pf (Sec a1 a2) = sf (foldA ef mf sf pf a1) (foldA ef mf sf pf a2)
foldA ef mf sf pf (Par a1 a2) = pf (foldA ef mf sf pf a1) (foldA ef mf sf pf a2)

recA :: (Duracion -> b) -> (Duracion -> Accion a -> b) -> (b -> b -> Animacion a -> Animacion a -> b) -> (b -> b -> Animacion a -> Animacion a -> b) -> Animacion a -> b
recA ef mf sf pf (Espera i) = ef i
recA ef mf sf pf (Mov i ac) = mf i ac
recA ef mf sf pf (Sec a1 a2) = sf (recA ef mf sf pf a1) (recA ef mf sf pf a2) a1 a2
recA ef mf sf pf (Par a1 a2) = pf (recA ef mf sf pf a1) (recA ef mf sf pf a2) a1 a2

-- 5. Con esquemas.
duracion' :: Animacion a -> Int 
duracion' = foldA
    (\d -> d) -- id
    (\d ac -> d) -- const
    (\n1 n2 -> n1 + n2) -- +
    (\n1 n2 -> max n1 n2) -- max

alargar' :: Int -> Animacion a -> Animacion a
alargar' n = foldA
    (\d -> Espera (n*d)) -- Espera . (n*)
    (\d ac -> Mov (n*d) ac) -- Mov . (n*)
    (\a1 a2 -> Sec a1 a2) -- Sec
    (\a1 a2 -> Par a1 a2) -- Par

simular' :: Animacion a -> [Frame a]
simular' = foldA
    (\d -> replicate d [])
    (\d ac -> replicate d [ac])
    (\fs1 fs2 -> fs1 ++ fs2) -- ++
    (\fs1 fs2 -> zipLong fs1 fs2) -- zipLong

zipLong' :: [[a]] -> ([[a]] -> [[a]])
zipLong' yss = recr cr cb yss
    where
        cb yss = yss 
        cr xs xss r [] = xs:xss
        cr xs xss r (ys:yss') = (xs++ys) : r yss'

tiemposDeEspera' :: Animacion a -> [Tiempo]
tiemposDeEspera' = recA
    (\d -> contarHasta d) -- contarHasta
    (\_ _ -> [])
    (\ts1 ts2 a1 a2 -> ts1 ++ map (+ duracion a1) ts2) 
    (\ts1 ts2 a1 a2 -> combinarSinDuplicados ts1 ts2) 

recN :: (Int -> b) -> b -> Int -> b
recN f z 0 = z
recN f z n = f n (recN f z (n-1))

contarHasta' :: Int -> [Int]
contarHasta' = recN f z n
    where
        z = []
        f n r = r ++ [n]

-- 6. Utilizando esquemas.
-- a. Dada una animación describe un simulador en el que la animación se repite infinitamente.
ciclar :: Animacion a -> Simulador a 
ciclar a = let frames = simular a 
            in \n -> ciclar' (mod n (length frames)) frames

ciclar' :: Int -> [Frame a] -> Frame a 
ciclar' = flip (foldr (\fr frs n -> if n == 0 then fr else frs (n-1))
                        (const []))

-- b. Dadas dos listas con la misma longitud, describe una Animacion que consiste en la secuencia tal que la animación de cada posición de una lista ocurre en simultáneo con la correspondiente en la otra lista.
combinar :: [Animacion a] -> [Animacion a] -> Animacion a
combinar xs ys = secuenciar (paralelizar (zip xs ys))

paralelizar :: [(Animacion a, Animaciona a)] -> [Animacion a] 
paralelizar = map (\(a1, a2) -> Par a1 a2)

secuenciar :: [Animacion a] -> Animacion a
secuenciar = foldr (\a ar -> Sec a ar) (Espera 0)

-- c. Describe una lista de Frames con la duración dada, donde en cada tiempo se observa la superposición de los simuladores en la lista de entrada.
mezclar :: [Simulador a] -> Duracion -> [Frame a]
mezclar ss d = map (\t -> fusionar t ss) (contarHasta d)

fusionar :: Int -> [Simulador a] -> Frame a
fusionar t = foldr (\s r -> s t ++ r) []