-- 1. Recursion explicita.
-- a. Devuelve una tupla cuya primera componente es partition, compuesto por los elementos que cumplen y no cumplen el criterio dado, y la segunda componente son los elementos que cumplen luego de aplicarse la funcion del criterio. 
partition :: Criteria a b -> [a] -> (Partition a, [b])
partition (C p f g) [] = (([], []), [])
partition (C p f g) (x:xs) = let ((ys, zs), ws) = partition (C p f g) xs
    in if p x
        then ((x:ys, zs) f x : ws)
        else ((ys, x:zs) g x : ws)

-- b. Aplica el nuevo criterio a los elementos que no cumplieron criterios anteriores (la parte de la partición que quedó sin procesar), y luego combina los resultados nuevos con los previos usando la función combinadora.
step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])
step c f ((ys, zs), ws) = let ((ys', zs'), ws') = partition c fs
    in ((ts ++ ts', fs'), f ys' : ys)

-- c. Para un elemento a, primero evalúa el primer criterio y luego, sobre el resultado b obtenido, aplica el segundo criterio para finalmente obtener c.
composeC :: Criteria a b -> Criteria b c -> Criteria a c
composeC (C p1 f1 g1) (C p2 f2 g2) = C (\x -> p1 x && p2 (f1 x)) (f2 . f1) (g2 . g1)

-- 2. Recursion explicita.
-- a. Dado un funnel, una función que "reduce" (foldr) una lista de resultados, y una lista de tipo [a], retorna la particion de elementos a tras aplicar el funnel. 
appF :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])

-- b. Computa el complemento de un funnel que filtra y transforma infomacion con los criterios del funnel dado negados. Tener en cuenta que al negra un criterio, los predicados de transformacion deben intercambiarse adecuadamente.    
complementF :: Funnel a b -> Funnel a b

-- c. Dado un funnel, retorna uno donde los criterios se aplican al reves. Pensar la solucion como una lista 
reverseF :: Funnel a b -> Funnel a b

-- d. Dado un funnel y una funcion b -> c, lo retorna mapeando sus funciones de a -> b por a -> c
mapF :: (b -> c) -> Funnel a b -> Funnel a c

-- e. Une dos funnel, paso a paso. Cada paso del primer funnel alimenta de datos al segundo, y si hay mas pasos en alguno de los dos, se ignoran. 
zipF :: Funnel a b -> Funnel b c -> Funnel a c

-- 3. Esquema primitivo y recursivo de Funnel a b.
foldF :: (Criteria a b -> c) -> (Criteria a b -> c -> c) -> Funnel a b -> c 
foldF inf sf (Initial c) = inf c
foldF inf sf (Step c f) = sf c (foldF inf sf f)

recF :: (Criteria a b -> c) -> (Criteria a b -> Funnel a b -> c -> c) -> Funnel a b -> c 
recF inf sf (Initial c) = inf c
recF inf sf (Step c f) = sf c f (recF inf sf f)

-- 4. Utilizando esquemas.
partition :: Criteria a b -> [a] -> (Partition a, [b])

step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])

composeC :: Criteria a b -> Criteria b c -> Criteria a c

-- 5. Demostrar: para todo fn. para todo f. para todo xs. ¿appF fn f xs = appF (complementF (complementF fn)) f xs?