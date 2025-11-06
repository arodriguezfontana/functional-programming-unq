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


-- 4

-- 5

-- 6

-- 7
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : (map f xs)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x
    then x : filter p xs
    else filter p xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [] = error "La lista no puede ser vacía."
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

--
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z [] = [z]
scanr f z (x:xs) = f x (head (scanr f z xs)) : (scanr f z xs)

-- 8

-- 9
sum :: [Int] -> Int
sum = foldr (\n m -> n + m) 0

length :: a -> Int
length = foldr (\_ n -> 1 + n) 0

map :: (a -> b) -> [a] -> [b]
map g = foldr (\x ys -> g x : ys) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x ys -> if p x then x : ys else ys) []

find :: (a -> Bool) -> [a] -> Maybe a
find p = foldr (\x my -> if p x then Just x else my) Nothing

any :: (a -> Bool) -> [a] -> Bool
find p = foldr (\x b -> p x || b) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x b -> p x && b) True

countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldr (\x n -> if p x then 1 + n else n) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr (\x (ys,zs) -> if p x then (x:ys,zs) else (ys,x:zs)) ([],[])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]


scanr :: (a -> b -> b) -> b -> [a] -> [b]
takeWhile :: (a -> Bool) -> [a] -> [a]
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
elemAt :: Int -> [a] -> a

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