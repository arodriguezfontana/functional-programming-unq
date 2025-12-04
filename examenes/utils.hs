-- Lemas demostrados en clase.
if b then f x else f y = f (if b then x else y) -- Distr. del if
i* n > i* m then x else y = n > m then x else y -- Arit.
length (xs ++ ys) = length xs + length ys
count (const True) = length
elem = any . (==)
map f (xs ++ ys) = map f xs ++ map f ys
any (elem x) = elem x . concat
subset xs ys = all (flip elem ys) xs
all null = null . concat
length = length . reverse
reverse (xs ++ ys) = reverse ys ++ reverse xs
all p (xs++ys) = all p (reverse xs) && all p (reverse ys)
map id = id
map f . map g = map (f . g)
concat . map (map f) = map f . concat
foldr ((+) . suma') 0 = sum . map suma'
foldr f z . foldr (:) [] = foldr f z
foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs
(+1) . foldr (+) 0 = foldr (+) 1
many n f = foldr (.) id (replicate n f) siendo many 0 f = id
many n f = f . many (n - 1)
zipWith (flip f) xs ys = map (uncurry f) (flip zip xs ys)
(f x y) = g x (h y) entonces h . foldr f z = foldr g (h z)

-- Esquemas.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b -- Se utiliza cuando se debe usar el caso recursivo o se devuelve algo en el caso base.
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

recN :: (Int -> b) -> b -> Int -> b
recN f z 0 = z
recN f z n = f n (recN f z (n-1))

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT z f EmptyT = z
foldT z f (NodeT x t1 t2) = f x (foldT z f t1) (foldT z f t2))

recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT z f EmptT = z
recT z f (NodeT x t1 t2) = f x t1 t2 (recT z f t1) (recT z f t2)

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

foldC :: a -> -- Para hacer un esquema largo sin escribir tanto.
    (Variable -> String -> a) ->
    (Variable -> a) ->
    (a -> a -> a) ->
    (Exp -> a -> a -> a) ->
    Cmd ->
    a
foldC fsk fst fp fsq fi c = case c of
    Skip -> fsk
    StoreInput v s -> fst v s
    PrintVar v -> fp v
    Seq c1 c2 -> fsq (re c1) (re c2)
    If e c1 c2 -> fi e (re c1) (re c2)
  where
    re = foldCmd fsk fst fp fsq fi

-- Funciones con esquemas y algunas explicaciones.
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

evalMSE' :: Eq a => MSExp a -> a -> N -- Para evalMSE', el tipo de b es (a -> N) porque necesita recibir un valor a despues del multiset de tipo MSExp a.
evalMSE' = foldM -- Cuando le paso al fold las 4 funciones, devuelve el tipo MSExp a -> b, si le paso un multiset, devuelve b.
    (\_ -> Z) -- const Z
    (\n x y -> if x==y then n else Z)
    (\n r y -> sumarN n (r y)) -- r::(a -> N) (no (N)), y::(a).
    (\r1 r2 y-> sumarN (r1 y) (r2 y))

-- Funciones basicas.
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

-- Estructura para las demos.
por ppio. de ext., para todo :
    ¿?

sea  , quiero demostrar que, ppio. de ind. estructural sobre , es eq. a:
    cb, =:
        ¿?
    ci, =:
        hi: ¡!
        ti: ¿?

cbi:
    x
=      (y.n)
cbd:
cb demostrado.

cii:
cid:
ci demostrado.

-- Estructura para los lemas.
para todo :
    ¿?

sea , quiero demostrar que, por casos sobre :
    c1, x=Empty y=cualquier caso
    c2, x/=Empty y=Empty
    c3, x/=Empty y/=Empty

-- Funciones adicionales.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : zipConcat (listPerLevel t1) (listPerLevel t2)

zipConcat :: [[a]] -> [[a]] -> [[a]]
zipConcat [] ys = ys
zipConcat xs [] = xs
zipConcat (x:xs) (y:ys) = (x ++ y) : zipConcat xs ys

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT x _ _) = [x]
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT n EmptyT EmptyT) = [[n]]
todosLosCaminos (NodeT n ri rd) = (agregoACada n (todosLosCaminos ri)) ++ (agregoACada n (todosLosCaminos rd))

agregoACada :: a -> [[a]] -> [[a]]
agregoACada _ [] = []
agregoACada x (ys:yss) = (x : ys) : agregoACada x yss

caminoMasCortoHasta :: Eq a => a -> Tree a -> Maybe [Dir]
caminoMasCortoHasta e = foldT 
    Nothing 
    (\x r1 r2 -> if x == e 
        then Just [] 
        else elMasCorto (agregarPaso Izq r1) (agregarPaso Der r2))

agregarPaso :: Dir -> Maybe [Dir] -> Maybe [Dir]
agregarPaso _ Nothing = Nothing
agregarPaso d (Just camino) = Just (d : camino)

elMasCorto :: Maybe [a] -> Maybe [a] -> Maybe [a]
elMasCorto Nothing y = y
elMasCorto x Nothing = x
elMasCorto (Just p1) (Just p2) = 
    if length p1 <= length p2 then Just p1 else Just p2

estaEnElCamino :: [Dir] -> Tree a -> Bool
estaEnElCamino ds EmptyT = False
estaEnElCamino ds (NodeT n t1 t2) = case ds of
    [] -> True
    (Izq:ds) -> estaEnElCamino ds t1 
    (Der:ds) -> estaEnElCamino ds t2

estaEnElCamino' :: [Dir] -> Tree a -> Bool
estaEnElCamino' = flip (foldT
    (const False)
    (\n r1 r2 ds -> case ds of
            [] -> True
            (Izq:ds) -> r1 ds
            (Der:ds) -> r2 ds))

todosLosCaminosDeDirs :: Tree a -> [[Dir]]
todosLosCaminosDeDirs EmptyT = [[]]
todosLosCaminosDeDirs (NodeT n t1 t2) = map (Izq :) (todosLosCaminosDeDirs t1) ++ map (Der :) (todosLosCaminosDeDirs t1)

todosLosCaminosDeDirs' :: Tree a -> [[Dir]]
todosLosCaminosDeDirs' = foldT 
    []                                              
    (\_ r1 r2 -> [] : map (Izq:) r1 ++ map (Der:) r2)

todosLosCaminosDeDirsHasta :: Eq a => a -> Tree a -> [[Dir]]
todosLosCaminosDeDirsHasta _ EmptyT = []
todosLosCaminosDeDirsHasta e (NodeT x t1 t2) = 
    let izq = map (Izq:) (todosLosCaminosDeDirsHasta e t1)
        der = map (Der:) (todosLosCaminosDeDirsHasta e t2)
        caminosAbajo = izq ++ der in 
    if x == e 
        then [] : caminosAbajo 
        else caminosAbajo

todosLosCaminosDeDirsHasta' :: Eq a => a -> Tree a -> [[Dir]]
todosLosCaminosDeDirsHasta' e = foldT 
    []                                  
    (\x r1 r2 ->                        
        let caminosHijos = map (Izq:) r1 ++ map (Der:) r2 in 
            if x == e 
            then [] : caminosHijos 
            else caminosHijos)

losAntecesoresDe :: a -> Tree a -> [a] -- Bien pero no eficiente.
losAntecesoresDe _ EmptyT = []
losAntecesoresDe e (NodeT e' t1 t2) = if elemT e t1
    then e' : losAntecesoresDe e t1
    else if elemT e t2
        then e' : losAntecesoresDe e t2
        else []

elemT :: Eq a => a -> Tree a -> Bool
elemT _ EmptyT = False
elemT e (NodeT e' t1 t2) = (e == e') || (elemT e t1) || (elemT e t2)

losAntecesoresDe :: Eq a => a -> Tree a -> [a] -- Bien y eficiente.
losAntecesoresDe e t = case buscar e t of
    Just antecesores -> antecesores
    Nothing -> []

buscar :: Eq a => a -> Tree a -> Maybe [a]
buscar _ EmptyT = Nothing
buscar e (NodeT x t1 t2) = if x==e
    then Just []                  
    else case buscar e t1 of -- Miro izquierda.
        Just camino -> Just (x : camino)
        Nothing -> case buscar e t2 of  -- Miro derecha.
            Just camino -> Just (x : camino)
            Nothing     -> Nothing -- No estaba en ningún lado.

losAntecesoresDe :: Eq a => a -> Tree a -> [a]
losAntecesoresDe e t = 
    case foldT Nothing 
           (\x r1 r2 -> 
               if x == e 
               then Just []
               else case r1 of
                      Just izq -> Just (x : izq)
                      Nothing  -> case r2 of
                                    Just der -> Just (x : der)
                                    Nothing  -> Nothing
           ) t of
       Just camino -> camino
       Nothing -> []

sumCuadradosFold :: [Int] -> Int
sumCuadradosFold = foldr (\n m -> (n*n) + m) 0

subsetFold :: [a] -> [a] -> Bool
subsetFold = foldr
    (\x r -> \ys -> elem x ys && (r ys))
    (const True)

acumSum :: [Int] -> [Int]
acumSum [] = []
acumSum (n:ns) = let r = acumSum ns if -- La recursion primero se usa para recorrer desde el fondo y al usar let se obtiene eficiencia al hacer la recursion una sola vez.
    if null r
        then [n]
        else (n + head r) : r

acumSumFold :: [Int] -> [Int]
acumSumFold = foldr (\n ms -> if null ms 
    then [n]
    else (n + head ms) : ms) []

countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldr (\x n -> unoSi (p x) + n) 0

partition :: (a -> Bool) -> [a] -> ([a], [a]) -- Recorre una lista y la divide en dos en una tupla (a,b), donde en a cumplen p, y en b no.
partition f = foldr (\x (xs,ys) -> if f x then (x:xs, ys) else (xs, x:ys)) ([],[])

remove :: Eq a => a -> [a]
remove e = recr (\x xs rs -> if e == x then xs else x : rs) []

take' :: Int -> [a] -> [a]
take' = flip (foldr (\x xs n -> if n == 0 then [] else x : xs (n-1)) (const []))

drop' :: Int -> [a] -> [a]
drop' = flip (foldr (\x rs n -> if n == 0 then x : rs 0 else rs (n-1)) (const []))

elemAt' :: Int -> [a] -> a
elemAt' = flip (foldr (\x rs n -> if n == 0 then x else rs (n-1)) (const (error "")))

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

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta (n-1) ++ [n]

contarDesde :: Int -> [Int]
contarDesde 0 = []
contarDesde n = n : contarDesde (n-1)

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

singularSi :: a -> Bool -> [a]
singularSi x True = [x]
singularSi _ False = []