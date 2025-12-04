-- 1. Explicita.
-- a. Retorna el significado del multiset dado. 
evalMSE :: Eq a => MSExp a -> a -> N
evalMSE Empty y = Z
evalMSE (Entry n x) y = if x==y then n else Z
evalMSE (AddAll n m) y = sumarN n (evalMSE m y)
evalMSE (Union m1 m2) y = sumarN (evalMSE m1 y) (evalMSE m2 y)

sumarN :: N -> N -> N
sumarN Z m = m
sumarN (S n) m = S (sumarN n m)

-- b. Dada una funcion de actualziacion de numeros naturales y un multiset naturalizado, describe el multiset que resulta de actualizar todas las ocurrencias de los elementos actuales segun la funcion dada. Por ejemplo, si la funcion incrementa el natural en 1, es equivalente a que todos los elementos existentes incorporan una ocurrencia mas.
updateOcurrsMSE :: (N -> N) -> MSExp a -> MSExp a
updateOcurrsMSE f Empty = Empty
updateOcurrsMSE f (Entry n x) = Entry (f n) x
updateOcurrsMSE f (AddAll n m) = AddAll (f n) (updateOcurrsMSE f m) -- para un arbol normalizando no existe AddAll
updateOcurrsMSE f (Union m1 m2) = Union (updateOcurrsMSE f m1) (updateOcurrsMSE f m2)

-- c. Describe el resultado de la normalizacion del multiset dado. Un multiset esta normalizado si no contiene ocurrencias del contrstructor AddAll, y si ningun constructor Union tiene como argumento un constructor Empty.
normMSE :: MSExp a -> MSExp a
normMSE Empty = Empty
normMSE (Entry n x) = Entry n x
normMSE (AddAll n m) = updateOcurrsMSE (\v -> sumarN n v) (normMSE m)
normMSE (Union m1 m2) = normUnion (normMSE m1) (normMSE m2)

normAddAll :: N -> MSExp a -> MSExp a
normAddAll n Empty = Empty
normAddAll n (Entry n' x) = Entry (addN n n') x
normAddAll n (Union m1 m2) = Union (normAddAll f m1) (normAddAll f m2)

normUnion :: MSExp a -> MSExp a -> MSExp a 
normUnion Empty m2 = m2 
normUnion m1 Empty = m1 
normUnion m1 m2 = Union m1 m2 

-- d. Describe la cantidad de "fallas" que tiene el multiset dado para estar normalziado, (o sea, la cantidad de constructores AddAll ademas de la cantidad de constructores Empty que son argumento de una Union). Observar que si cantFallas m = 0, entonces m esta normalizado.
cantFallas :: MSExp a -> Int
cantFallas Empty = 0
cantFallas (Entry n x) = 0 
cantFallas (AddAll n ms) = 1 + cantFallas ms 
cantFallas (Union m1 m2) = unoSiHayEmpty m1 m2 + cantFallas m1 + cantFallas m2 
 
unoSiHayEmpty :: MSExp a -> MSExp a -> Int 
unoSiHayEmpty Empty m2 = 1 
unoSiHayEmpty m1 Empty = 1 
unoSiHayEmpty m1 m2 = 0 

-- 2. Esquema recursivo y primitivo de MSExp a. 
foldM :: b -> (N -> a -> b) -> (N -> b -> b) -> (b -> b -> b) -> MSExp a -> b
foldM z ef af uf Empty = z
foldM z ef af uf (Entry n x) = ef n x 
foldM z ef af uf (AddAll n ms) = af n (foldM z ef af uf ms)
foldM z ef af uf (Union m1 m2) = uf (foldM z ef af uf m1) (foldM z ef af uf m2)

recM :: b -> (N -> a -> b) -> (N -> MSExp a -> b -> b) -> (MSExp a -> MSExp a -> b -> b -> b) -> MSExp a -> b
recM z ef af uf Empty = z
recM z ef af uf (Entry n x) = ef n x 
recM z ef af uf (AddAll n ms) = af n ms (recM z ef af uf ms)
recM z ef af uf (Union m1 m2) = uf m1 m2 (recM z ef af uf m1) (recM z ef af uf m2)

-- 3. Con esquemas.
evalMSE' :: Eq a => MSExp a -> a -> N -- el tipo de b es (a->N) porque evalMSE necesita recibir un valor a despues de la estructura.
evalMSE' = foldM
    (\_ -> Z) -- const Z
    (\n x y -> if x==y then n else Z)
    (\n r y -> sumarN n (r y))
    (\r1 r2 y-> sumarN (r1 y) (r2 y))

updateOcurrsMSE' :: (N -> N) -> MSExp a -> MSExp a
updateOcurrsMSE' f = foldM
    Empty
    (\n x -> Entry (f n) x)
    (\n m -> AddAll (f n) m)
    (\m1 m2 -> Union m1 m2)

normMSE' :: MSExp a -> MSExp a
normMSE' = foldM
    Empty
    (\n x -> Entry n x) -- Entry
    (\n m -> updateOcurrsMSE (\v -> sumarN n v) m)
    (\m1 m2 -> normUnion m1 m2) -- normUnion

cantFallas' :: MSExp a -> Int
cantFallas' = recM
    0
    (\n x -> 0)
    (\n m mr -> 0 + mr)
    (\n1 n2 nr1 nr2 -> unoSiHayEmpty m1 m2 + mr1 + mr2)

-- 4. Demostrar: cantFallas . normMSE = const 0.
por ppio. de ext., para todo m':
    ¿(cantFallas . normMSE) m' = const 0 m'?
por def. de (.), es eq. a:
    ¿cantFallas (normMSE m') = const 0 m'?
por def. de const, es eq. a:
    ¿cantFallas (normMSE m') = 0?

sea m::MSExp a, quiero demostrar que:
    ¿cantFallas (normMSE m) = 0?

por ppio. de ind. estructural sobre, es eq. a:
    cb2, m=Empty:
        ¿cantFallas (normMSE Empty) = 0?
    cb2, m=Entry n x:
        ¿cantFallas (normMSE (Entry n x)) = 0?
    ci, m=AddAll n m1:
        hi: ¡cantFallas (normMSE m1) = const 0 m1!
        ti: ¿cantFallas (normMSE (AddAll n m1)) = 0?
    ci, m=Union m1 m2:
        hi1: ¡cantFallas (normMSE m1) = const 0 m1!
        hi2: ¡cantFallas (normMSE m2) = const 0 m2!
        ti: ¿cantFallas (normMSE (Union m1 m2)) = 0?

cb1i:
cantFallas (normMSE Empty)
-- normMSE.1
cantFallas Empty
-- cantFallas.1
0

cb1 demostrado.

cb2i:
cantFallas (normMSE (Entry n x))
-- normMSE.2
cantFallas (Entry n x)
-- cantFallas.2
0

cb2 demostrado.

ci1i:
cantFallas (normMSE (AddAll n m1))
-- normMSE.3
cantFallas (updateOcurrsMSE (\v -> sumarN n v) (normMSE m1))
-- lema 1
cantFallas (normMSE m1)
-- hi
const 0 m1
-- const
0

ci1 demostrado.

ci2i:
cantFallas (normMSE (Union m1 m2))
-- normMSE.4
cantFallas (normUnion (normMSE m1) (normMSE m2))
-- lema 2
cantFallas (normMSE m1) + cantFallas (normMSE m2)
-- hi1 y hi2
0 + 0 
-- aritm.
0

ci2 demostrado.

-- lema 1: cantFallas (updateOcurrsMSE f m) = cantFallas m
para todo m', para todo f':
    ¿cantFallas (updateOcurrsMSE f m') = cantFallas m'?

sea m::MSExp a, ya normalizado, f::N->N, quiero demostrar que, por ppio. de ind. estructural sobre m:
    cb1, m=Empty:
        ¿cantFallas (updateOcurrsMSE f Empty) = cantFallas Empty?
    cb2, m=Entry n x:
        ¿cantFallas (updateOcurrsMSE f (Entry n x)) = cantFallas (Entry n x)?
    cb2, m=Union m1 m2:
        hi1: ¡cantFallas (updateOcurrsMSE f m1) = cantFallas m1!
        hi2: ¡cantFallas (updateOcurrsMSE f m2) = cantFallas m2!
        ti: ¿cantFallas (updateOcurrsMSE f (Union m1 m2)) = cantFallas (Union m1 m2)?

cb1i:
cantFallas (updateOcurrsMSE f Empty)
-- updateOcurrsMSE.1
cantFallas Empty
-- cantFallas.1
0

cb2i:
cantFallas Empty
-- cantFallas.1
0

cb2i:
cantFallas (updateOcurrsMSE f (Entry n x))
-- updateOcurrsMSE.2
cantFallas (Entry (f n) x)
-- cantFallas.2
0

cb2d:
cantFallas (Entry n x)
-- cantFallas.2
0

cii:
cantFallas (updateOcurrsMSE f (Union m1 m2))
-- updateOcurrsMSE.4
cantFallas (Union (updateOcurrsMSE f m1) (updateOcurrsMSE f m2))
-- cantFallas.4
unoSiHayEmpty m1 m2 + cantFallas (updateOcurrsMSE f m1) + cantFallas (updateOcurrsMSE f m1)
-- hi1 y hi2
unoSiHayEmpty m1 m2 + cantFallas m1 + cantFallas m2

cid:
cantFallas (Union m1 m2)
-- cantFallas.4
unoSiHayEmpty m1 m2 + cantFallas m1 + cantFallas m2

-- lema 2: cantFallas (normUnion m1 m2) = cantFallas m1 + cantFallas m2
para todo m1', para todo m2':
    ¿cantFallas (normUnion m1' m2') = cantFallas m1' + cantFallas m2'?

sean m1, m2::MSExp a, ya normalizados, quiero demostrar que, por casos sobre m1 y m2:
    c1, m1=Empty, m2=cualquier caso
        ¿cantFallas (normUnion Empty m2) = cantFallas Empty + cantFallas m2?
    c2, m1/=Empty, m2=Empty
        ¿cantFallas (normUnion m1 Empty) = cantFallas m1 + cantFallas Empty?
    c3, m1/=Empty, m2/=Empty
        ¿cantFallas (normUnion m1 m2) = cantFallas m1 + cantFallas m2?

c1i:
cantFallas (normUnion Empty m2)
-- normUnion.1
cantFallas m2

c1d:
cantFallas Empty + cantFallas m2
-- cantFallas.1
0 + cantFallas m2
-- aritm.
cantFallas m2

c1 demostrado.

c2i:
cantFallas (normUnion m1 Empty)
-- normUnion.2
cantFallas m1 

c2d:
cantFallas m1 + cantFallas Empty
-- cantFallas.1
cantFallas m1 + 0
-- aritm.
cantFallas m1

c2 demostrado.

c3i:
cantFallas (normUnion m1 m2)
-- normUnion.3
cantFallas (Union m1 m2) 
-- cantFallas.4
unoSiHayEmpty m1 m2 + cantFallas m1 + cantFallas m2 
-- m1 y m2 no son Emty
0 + cantFallas m1 + cantFallas m2 
-- aritm.
cantFallas m1 + cantFallas m2 

c3d:
cantFallas m1 + cantFallas m2

c3 demostrado.