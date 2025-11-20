-- 1. Explicita.
-- a. Devuelve una tupla cuya primera componente es partition, compuesto por los elementos que cumplen y no cumplen el criterio dado, y la segunda componente son los elementos que cumplen luego de aplicarse la funcion del criterio. 
partition :: Criteria a b -> [a] -> (Partition a, [b]) -- (([a], [a]), [b]) 
partition (C p f g) [] = (([], []), [])
partition (C p f g) (x:xs) = let ((ys, zs), ws) = partition (C p f g) xs -- resultado de la recursion
    in if p x -- si se cumple, proceso el primer elemento
        then ((x:ys, zs) f x : ws)
        else ((ys, x:zs) g x : ws)

partition' :: Criteria a b -> [a] -> (Partition a, [b]) -- (([a], [a]), [b]) 
partition' (C p f g) = foldr (\x ((ts, fs), ys) -> if p x
                                                    then ((x:ts, fs), f x : ys)
                                                    else ((ts, x:fs), g x : ys)) 
                             (([], []), [])

-- b. Aplica el nuevo criterio a los elementos que no cumplieron criterios anteriores (la parte de la partición que quedó sin procesar), y luego combina los resultados nuevos con los previos usando la función combinadora.
step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])
step c f ((ys, zs), ws) = let ((ys', zs'), ws') = partition c fs -- los elementos de fs procesados con el nuevo criterio 
    in ((ts ++ ts', fs'), f ys' : ys)

-- c. Para un elemento a, primero evalúa el primer criterio y luego, sobre el resultado b obtenido, aplica el segundo criterio para finalmente obtener c.
composeC :: Criteria a b -> Criteria b c -> Criteria a c
composeC (C p1 f1 g1) (C p2 f2 g2) = C (\x -> p1 x && p2 (f1 x)) (f2 . f1) (g2 . g1)

-- 2. Explicita.
-- a. Dado un funnel, una función que "reduce" (foldr) una lista de resultados, y una lista de tipo [a], retorna la particion de elementos a tras aplicar el funnel. 
appF :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF (Initial c) f xs = let (ps, ys) = partition c xs 
                            in (ps, [f ys]) 
appF (Step c fu) f xs = step c f (appF fu f xs)

appF' :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF' = foldF (\c -> \g -> \xs -> let (ps, ys) = partition c xs 
                                            in (ps, [g ys]))
              (\c fr -> \g -> \xs -> step c g (fr g xs))

-- b. Computa el complemento de un funnel que filtra y transforma infomacion con los criterios del funnel dado negados. Tener en cuenta que al negra un criterio, los predicados de transformacion deben intercambiarse adecuadamente.    
complementF :: Funnel a b -> Funnel a b
complementF = foldF (Initial . complementC)
                    (Step . complementC)

complementC :: Criteria a b -> Criteria a b 
complementC (C p f g) = C (not . p) g f

-- c. Dado un funnel, retorna uno donde los criterios se aplican al reves. Pensar la solucion como una lista 
reverseF :: Funnel a b -> Funnel a b
reverseF (Initial c) = Initial c 
reverseF (Step c fu) = appendF (reverseF fu) (Initial c)

appendF :: Funnel a b -> Funnel a b -> Funnel a b 
appendF (Initial c) f2 = Step c f2 
appendF (Step c fu) f2 = Step c (appendF fu f2) 

-- d. Dado un funnel y una funcion b -> c, lo retorna mapeando sus funciones de a -> b por a -> c
mapF :: (b -> c) -> Funnel a b -> Funnel a c
mapF f (Initial c) = Initial (mapC f c)
mapF f (Step c fu)  = Step (mapC f c) (mapF f fu)

mapC :: (b -> c) -> Criteria a b -> Criteria a c
mapC f (C p g h) = C p (f . g) (f . h)

-- e. Une dos funnel, paso a paso. Cada paso del primer funnel alimenta de datos al segundo, y si hay mas pasos en alguno de los dos, se ignoran. 
zipF :: Funnel a b -> Funnel b c -> Funnel a c
zipF (Initial c1) f2  = case f2 of 
                          Initial c2 -> Initial (composeC c1 c2)
                          Step c2 fu -> Initial (composeC c1 c2)
zipF (Step c1 fu1) f2 = case f2 of 
                          Initial c2 -> Initial (composeC c1 c2)
                          Step c2 fu2 -> Step (composeC c1 c2) (zipF fu1 fu2)

-- 3. Esquema primitivo y recursivo de Funnel a b.
foldF :: (Criteria a b -> c) -> (Criteria a b -> c -> c) -> Funnel a b -> c 
foldF inf sf (Initial c) = inf c
foldF inf sf (Step c f) = sf c (foldF inf sf f)

recF :: (Criteria a b -> c) -> (Criteria a b -> Funnel a b -> c -> c) -> Funnel a b -> c 
recF inf sf (Initial c) = inf c
recF inf sf (Step c f) = sf c f (recF inf sf f)

-- 4. Utilizando esquemas.
appF' :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF' = foldF
        (\c h f xs-> step c f (h f xs))
        (\c f xs -> let (ys, zs) = partition' c xs in (ps, [f ys]))
