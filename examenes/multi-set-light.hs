-- 1. Recursion explicita.
-- a. Retorna el significado del multiset dado. 
evalMSE :: Eq a => MSExp a -> a -> N

-- b. Dada una funcion de actualziacion de numeros naturales y un multioset naturalizado, describe el multiset que resulta de actualizar todas las ocurrencias de los elementos actuales segund  la funcion dada. Por ejemplo, si la funcion incrementa el natural en 1, es equivalente a que todos los elementos existentes incorporan una ocurrencia mas.
updateOcurrsMSE :: (N -> N) -> MSExp a -> MSExp a

-- c. Describe el resultado de la normalizacion del multiset dado. Un multiset esta normalizado si no contiene ocurrencias del contrstructor AddAll, y si ningun constructor Union tiene como argumento un constructor Empty.
normMSE :: MSExp a -> MSExp a

-- d.  Describe la cantidad de "fallas" que tiene el multiset dado para estar normalziado, (o sea, la cantidad de constructores AddAll ademas de la cantidad de constructores Empty que son argumento de una Union). Observar que si cantFallas m = 0, entonces m esta normalizado.
cantFallas :: MSExp a -> Int

-- 2. Esquema primitivo y recursivo de MSExp a. 
foldM :: b -> (N -> a -> b) -> (N -> b -> b) -> (b -> b -> b) -> MSExp a -> b
foldM z ef af uf Empty = z
foldM z ef af uf (Entry n x) = ef n x 
foldM z ef af uf (AddAll n ms) = af n (foldM z ef af uf ms)
foldM z ef af uf (Union ms1 ms2) = uf (foldM z ef af uf ms1) (foldM z ef af uf ms2)

recM :: b -> (N -> a -> b) -> (N -> MSExp a -> b -> b) -> (MSExp a -> MSExp a -> b -> b -> b) -> MSExp a -> b
recM z ef af uf Empty = z
recM z ef af uf (Entry n x) = ef n x 
recM z ef af uf (AddAll n ms) = af n ms (recM z ef af uf ms)
recM z ef af uf (Union ms1 ms2) = uf ms1 ms2 (recM z ef af uf ms1) (recM z ef af uf ms2)

-- 3. 

-- 4. Demostrar: cantFallas . normMSE = const 0.
