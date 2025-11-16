-- lemas utiles
. if b then f x else f y = f (if b then x else y) -- dist. de if
-- i* n > i* m = n > m (aritm.)
. length (xs ++ ys) = length xs + length ys
. count (const True) = length
. elem = any . (==)
. map f (xs ++ ys) = map f xs ++ map f ys

-- lemas demostrados en las practicas
. any (elem x) = elem x . concat
. subset xs ys = all (flip elem ys) xs
. all null = null . concat
. length = length . reverse
. reverse (xs ++ ys) = reverse ys ++ reverse xs
. all p (xs++ys) = all p (reverse xs) && all p (reverse ys)
. map id = id
. map f . map g = map (f . g)
. concat . map (map f) = map f . concat
. foldr ((+) . suma') 0 = sum . map suma'
. foldr f z . foldr (:) [] = foldr f z
. foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs
. (+1) . foldr (+) 0 = foldr (+) 1
. many n f = foldr (.) id (replicate n f) siendo many 0 f = id
. many n f = f . many (n - 1)
. zipWith (flip f) xs ys = map (uncurry f) (flip zip xs ys)
. (f x y) = g x (h y) entonces h . foldr f z = foldr g (h z)

-- esquemas
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b -- cuando uso el caso recursivo o devuelvo algo en el caso base
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

recN :: (Int -> b) -> b -> Int -> b
recN f z 0 = z
recN f z n = f n (recN f z (n-1))

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT z f EmptT = z
foldT z f (fodeT x t1 t2) = f x (foldT z f t1) (foldT z f t2)

recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT z f EmptT = z
recT z f (NodeT x t1 t2) = f x t1 t2 (recT z f t1) (recT z f t2)

-- otros esquemas
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

zipWith :: [[a]] -> [[a]] -> [[a]]
zipWith (xs:xss) (ys:yss) = (xs++ys) : zipWith xss yss
zipWith _ _ = []

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter  f (x:xs) = if f x then x : filter f xs else filter f xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ z []     = [z]
scanr f z (x:xs) = let (y:ys) = scanr f z xs
    in f x y : y : ys

-- funciones recursivas utiles
zipLong :: [[a]] -> [[a]] -> [[a]]
zipLong [] yss = yss
zipLong xss [] = xss
zipLong (xs:xss) (ys:yss) = (xs++ys) : zipLong xss yss

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([], [])
unzip ((x,y):xys) = let (xs, ys) = (unzip xys) 
                        in (x:xs, y:ys)

replicate :: Int -> a -> [a]
replicate 0 n = []
replicate n x = x : replicate (n-1) x

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta (n-1) ++ [n]

contarDesde :: Int -> [Int]
contarDesde 0 = []
contarDesde n = n : contarDesde (n-1)

-- funciones con esquemas
zipLong' :: [[a]] -> ([[a]] -> [[a]])
zipLong' = recr
    (\yss -> yss)
    (\xs xss r yss -> case yss of
        [] -> xs:xss
        (ys:yss') -> (xs++ys) : r yss')

zipLong'' :: [[a]] -> [[a]] -> [[a]]
zipLong'' yss = recr cb cr yss
    where
        cb yss = yss -- zipLong [] yss = yss
        cr xs xss r [] = xss --  zipLong (xs:xss) [] = xss
        cr xs xss r (ys:yss') = (xs++ys) : r yss' -- zipLong (xs:xss) (ys:yss) = (xs++ys) : (zipLong xss) yss
        -- r no es [[a]], es [[a]] -> [[a]]

take' :: Int -> [a] -> [a]
take' n xs = foldr cr cb xs n
    where
        cb n = []
        cr x r 0 = []
        cr x r n = x : r (n-1) -- r hace la recursion al aplicarlo

-- funciones utiles
id :: a -> a
id x = x

apply :: (a -> b) -> a -> b
apply f x = f x

const :: a -> b -> a
const x y = x

twice :: (a -> a) -> a -> a
twice f x = f (f x)

unflip :: (b -> a -> c) -> a -> b -> c
unflip f x y = f y x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = (f x) (g x)

compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f (g x)

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

-- demos
por principio de extensionalidad, para todo :
    ¿?
por definicion de (.), es equivalente a:

sea  , quiero ver que:
    ¿?

por principio de induccion estructural sobre la estructura de , es equivalente a demostrar:
    cb, =:
        ¿?
    ci, =:
        hi: ¡!
        ti: ¿?

cb-izq:
cb-der:
cb demostrado.

ci-izq:
ci-der:
ci demostrado.

-- funciones de listas 
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

sum :: [Int] -> Int
sum [] = 0
sum (n:ns) = n + sum ns

product :: [Int] -> Int
product [] = 0
product (n:ns) = n * product ns

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs) = e == x || elem e xs

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = f x || any f xs

count :: (a -> Bool) -> [a] -> Int
count f [] = 0
count f (x:xs) = if f x
    then 1 + count f xs
    else count f xs

subset :: Eq a => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : xs ++ ys

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : (zip xs ys)

unzip :: [(a,b)] -> ([a],[b])
unzip [] = []
unzip ((x,y):xys) = let (xs, ys) = (unzip xys) 
                        in (x:xs, y:ys)

-- funciones de arboles

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmptyT = False 
anyT f (NodeT x t1 t2) = f x || anyT f t1 || any f t2

countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT = 0
count f (NodeT x t1 t2) = if f x
    then 1 + count f t1 + count f t2
    else count f t1 + count f t2

countLeaves :: Tree a -> Int
countLeaves EmptyT = 1
countLeaves (NodeT _ t1 t2) = countLeaves t1 + countLeaves t2

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT n t1 t2) = inOrder t1 ++ [n] ++ inOrder t2

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
laDeMayorLongitud xs ys = if longitud xs >= longitud ys
                           then xs
                           else ys

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT n EmptyT EmptyT) = [[n]]
todosLosCaminos (NodeT n ri rd) = (agregoACada n (todosLosCaminos ri)) ++ (agregoACada n (todosLosCaminos rd))

agregoACada :: a -> [[a]] -> [[a]]
agregoACada _ [] = []
agregoACada x (ys:yss) = (x : ys) : agregoACada x yss

-- adicionales

losAntecesoresDe :: a -> Tree a -> [a]
losAntecesoresDe _ EmptyT = []
losAntecesoresDe e (NodeT e' t1 t2) = if elemT e t1
    then e' : losAntecesoresDe e t1
    else if elemT e t2
        then e' : losAntecesoresDe e t2
        else []

elemT :: Eq a => a -> Tree a -> Bool
elemT _ EmptyT = False
elemT e (NodeT e' t1 t2) = (e == e') || (elemT e t1) || (elemT e t2)

sumCuadrados :: [Int] -> Int
sumCuadrados [] = 0
sumCuadrados (n:ns) = (n*n) + sumCuadrados ns

sumCuadradosFold :: [Int] -> Int
sumCuadradosFold = foldr (\n ms -> (n*n) + ms) 0

subset :: [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys

subsetFold :: [a] -> [a] -> Bool
subsetFold = foldr (\x h -> \ys -> elem x ys && (h ys)) (const True)

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
 
append :: [a] -> [a] -> [a]
append [] = \ys -> ys
append (x:xs) = \ys -> x : append xs ys

appendFold :: [a] -> [a] -> [a]
appendFold = foldr (\x h -> \ys -> x : h ys) (\ys -> ys)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map f xs)

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x ys -> (f x) : ys) []

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\x n -> unoSi (f x) + n) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\x (xs,ys) -> if f x then (x:xs, ys) else (xs, x:ys)) ([],[])

take' :: Int -> [a] -> [a]
take _ [] = []
take n (x:xs) = if n == 0 then [] else x : take (n-1) xs

take' :: Int -> [a] -> [a]
take' = flip (foldr (\x xs n -> if n == 0 then [] else x : xs (n-1)) (const []))

drop' :: Int -> [a] -> [a]
drop _ [] = []
drop n (x:xs) = if n == 0 then x:xs else drop (n-1) xs

drop' :: Int -> [a] -> [a]
drop' = flip (foldr (\x rs n -> if n == 0 then x : rs 0 else rs (n-1)) (const []))

elemAt :: Int -> [a] -> a
elemAt = flip (foldr (\x rs n -> if n == 0 then x else rs (n-1)) (const (error "F")))

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x m -> if f x then Just x else m) Nothing

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

singularSi :: a -> Bool -> [a]
singularSi x True = [x]
singularSi _ False = []