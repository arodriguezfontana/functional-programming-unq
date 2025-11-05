-- 1
cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen f (Capa i p) = if f i
    then 1 + cantidadCapasQueCumplen f p
    else cantidadCapasQueCumplen f p

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza = Prepizza
soloLasCapasQue f (Capa i p) = if f i
    then Capa i (soloLasCapasQue f p)
    else soloLasCapasQue f p

-- 2
esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

sinLactosa :: Pizza -> Pizza
sinLactosa p = soloLasCapasQue (not . esQueso) pizza

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa p = (==) 0 (cantidadCapasQueCumplen esQueso p)

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso p = cantidadCapasQueCumplen esQueso p

duplicarSiAceituna :: Ingrediente -> Ingrediente
duplicarSiAceituna (Aceituna n) = Aceituna (n*2)
duplicarSiAceituna i = i

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas p = conCapasTransformadas duplicarSiAceituna p

-- 3
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f z Prepizza = z
pizzaProcesada f z (Capa i p) = f i (pizzaProcesada f z p)

-- 4

-- 5

-- 6

-- 7

-- 8

-- 9

-- 10

-- a1
-- a
sumCuadrados :: [Int] -> Int
sumCuadrados [] = 0
sumCuadrados (n:ns) = (n*n) + sumCuadrados ns

sumCuadradosFold :: [Int] -> Int
sumCuadradosFold = foldr (\n ms -> (n*n) + ms) 0

-- b
subset :: [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys

subsetFold :: [a] -> [a] -> Bool
subsetFold = foldr (\x h -> \ys -> elem x ys && (h ys)) (const True)

-- c
acumSum :: [Int] -> [Int]
acumSum [] = []
acumSum (n:ns) = let r = acumSum ns if
    if null r
        then [n]
        else (n + head r) : r

acumSumFold :: [Int] -> [Int]
acumSumFold = foldr (\n ms -> if null ms 
    then [n]
    else (n + head ms) : ms) []

-- d 
append :: [a] -> [a] -> [a]
append [] = \ys -> ys
append (x:xs) = \ys -> x : append xs ys

appendFold :: [a] -> [a] -> [a]
appendFold = foldr (\x h -> \ys -> x : h ys) (\ys -> ys)

-- e
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map f xs)

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x ys -> (f x) : ys) []