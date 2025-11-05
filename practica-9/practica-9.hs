-- 1
-- a
evalEA :: EA -> Int
evalEA (Const n) = n
evalEA (BOp binop e1 e2) = case binop of
    Sum -> (evalEA e1) + (evalEA e2)
    Mul -> (evalEA e1) * (evalEA e2)

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp binop e1 e2) = case binop of
     Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
     Mul -> Prod (ea2ExpA e1) (ea2ExpA e2)

expA2ea :: ExpA -> EA
expA2ea (Cte n) = Const n
expA2ea (Suma e1 e2) = BOp Sum (ea2ExpA e1) (ea2ExpA e2)
expA2ea (Prod e1 e2) = BOp Mul (ea2ExpA e1) (ea2ExpA e2)

-- b
-- b1
por ppio. de ext.:
para todo e:
    ¿(ea2ExpA . expA2ea) e = id e?
-- (.)
    ¿ea2ExpA (expA2ea e) = id e?
-- id
    ¿ea2ExpA (expA2ea e) = e?

sea e' un elemento cualquierade tipo ExpA.
quiero ver que:
    ¿ea2ExpA (expA2ea e') = e'?

por ppio. de ind. sobre la estructura e',
es eq. a demostrar:
    CB, e'=Cte n:
        ¿ea2ExpA (expA2ea (Cte n)) = Cte n?
    CI1, e'=(Suma e1 e2):
        HI1: ¡ea2ExpA (expA2ea e1) = e1!
        HI2: ¡ea2ExpA (expA2ea e2) = e2!        
        TI: ¿ea2ExpA (expA2ea (Suma e1 e2)) = Suma e1 e2?
    CI2, e'=(Prod e1 e2):
        HI1: ¡ea2ExpA (expA2ea e1) = e1!
        HI2: ¡ea2ExpA (expA2ea e2) = e2!        
        TI: ¿ea2ExpA (expA2ea (Prod e1 e2)) = Prod e1 e2?

CB.i:
ea2ExpA (expA2ea (Cte n))
-- expA2ea.1
ea2ExpA (Const n)
-- ea2ExpA.1
Cte n

CB.d:
Cte n

cb demostrado.

CI1.i:
ea2ExpA (expA2ea (Suma e1 e2))
-- expA2ea.2
ea2ExpA (BOp Sum (expA2ea e1) (expA2ea e2))
-- ea2ExpA.2
case Sum of
     Sum -> Suma (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
     Mul -> Prod (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
-- case of.1
Suma (ea2ExpA (ea2ExpA e1)) (ea2ExpA (ea2ExpA e2))
-- h1.1
Suma e1 (ea2ExpA (ea2ExpA e2))
-- h1.2
Suma e1 e2

CI1.d:
Suma e1 e2

ci1 demostrado.

CI2.i:
ea2ExpA (expA2ea (Prod e1 e2))
-- expA2ea.2
ea2ExpA (BOp Mul (expA2ea e1) (expA2ea e2))
-- ea2ExpA.2
case Mul of
     Sum -> Suma (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
     Mul -> Prod (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
-- case of.2
Prod (ea2ExpA (ea2ExpA e1)) (ea2ExpA (ea2ExpA e2))
-- h2.1
Prod e1 (ea2ExpA (ea2ExpA e2))
-- h2.2
Prod e1 e2

CI2.d:
Prod e1 e2

ci2 demostrado.

-- b2
por ppio. de ext.:
para todo e:
    ¿expA2ea . ea2ExpA e = id e?
-- (.)
    ¿expA2ea (ea2ExpA e) = id e?
-- id
    ¿expA2ea (ea2ExpA e) = e?

sea e' un elemento de tipo EA cualquiera,
quiero ver que:
    ¿expA2ea (ea2ExpA e') = e'?

por ppio. de ind. sobre la estructura e',
es eq. a demostrar:
    CB, e'=(Const n):
        ¿expA2ea (ea2ExpA (Const n)) = Const n?
    CI1, e'=(BOp Sum e1 e2):
        HI1: ¡expA2ea (ea2ExpA e1) = e1!
        HI2: ¡expA2ea (ea2ExpA e2) = e2!
        TI: ¿expA2ea (ea2ExpA (BOp Sum e1 e2)) = BOp Sum e1 e2?
    CI2, e'=(BOp Mul e1 e2):
        HI1: ¡expA2ea (ea2ExpA e1) = e1!
        HI2: ¡expA2ea (ea2ExpA e2) = e2!
        TI: ¿expA2ea (ea2ExpA (BOp Mul e1 e2)) = BOp Mul e1 e2?

CB.i:
expA2ea (ea2ExpA (Const n))
-- ea2ExpA.1
expA2ea (Cte n)
-- expA2ea.1
Const n

CB.d:
Const n

cb demostrado.

CI1.i:
expA2ea (ea2ExpA (BOp Sum e1 e2))
-- ea2ExpA.2
expA2ea (case Sum of
     Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
     Mul -> Prod (ea2ExpA e1) (ea2ExpA e2))
-- case of.1
expA2ea (Suma (ea2ExpA e1) (ea2ExpA e2))
-- expA2ea.2
BOp Sum (expA2ea (ea2ExpA e1)) (expA2ea (ea2ExpA e2))
-- hi1.1
BOp Sum e1 (expA2ea (ea2ExpA e2))
-- hi1.2
BOp Sum e1 e2

CI1.d:
BOp Sum e1 e2

ci1 demostrado.

CI2.i:
expA2ea (ea2ExpA (BOp Mul e1 e2))
-- ea2ExpA.2
expA2ea (case Mul of
     Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
     Mul -> Prod (ea2ExpA e1) (ea2ExpA e2))
-- case of.2
expA2ea (Prod (ea2ExpA e1) (ea2ExpA e2))
-- expA2ea.3
BOp Mul (expA2ea (ea2ExpA e1)) (expA2ea (ea2ExpA e2))
-- hi2.1
BOp Mul e1 (expA2ea (ea2ExpA e2))
-- hi2.2
BOp Mul e1 e2

CI1.d:
BOp Mul e1 e2

ci demostrado.

-- b3 
por ppio. de ext.:
para todo e:
    ¿(evalExpA . ea2ExpA) e = evalEA e?
-- (.)
    ¿evalExpA (ea2ExpA e) = evalEA e?

sea e' un elemento de tipo EA cualquiera,
quiero ver que:
    ¿evalExpA (ea2ExpA e') = evalEA e'?

por ppio. de ind. sobre la estructura e',
es eq. a demostrar:
    CB, e'=(Const n):
        ¿evalExpA (ea2ExpA (Const n)) = evalEA Const n?
    CI1, e'=(BOp Sum e1 e2):
        HI1: ¡evalExpA (ea2ExpA e1) = evalEA e1!
        HI2: ¡evalExpA (ea2ExpA e2) = evalEA e2!
        TI: ¿evalExpA (ea2ExpA (BOp Sum e1 e2)) = evalEA (BOp Sum e1 e2)?
    CI1, e'=(BOp Mul e1 e2):
        HI1: ¡evalExpA (ea2ExpA e1) = evalEA e1!
        HI2: ¡evalExpA (ea2ExpA e2) = evalEA e2!
        TI: ¿evalExpA (ea2ExpA (BOp Mul e1 e2)) = evalEA (BOp Mul e1 e2)?

CB.i:
evalExpA (ea2ExpA (Const n))
-- ea2ExpA
evalExpA (Cte n)
-- evalExpA
n

CB.d:
evalEA (Const n)
-- evalEA
n

cb demostrado.

CI1.i:
evalExpA (ea2ExpA (BOp Sum e1 e2))
-- ea2ExpA
evalExpA (case Sum of
     Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
     Mul -> Prod (ea2ExpA e1) (ea2ExpA e2))
-- case of.1
evalExpA (Suma (ea2ExpA e1) (ea2ExpA e2))
-- evalExpA
evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)
-- hi1.1 y hi1.2
evalEA e1 + evalEA e2

CI1.d:
evalEA (BOp Sum e1 e2)
-- evalEV
case Sum of
    Sum -> evalEA e1 + evalEA e2
    Prod -> evalEA e1 * evalEA e2
-- case of.1
evalEA e1 + evalEA e2

ci1 demostrado.

CI2.i:
evalExpA (ea2ExpA (BOp Mul e1 e2))
-- ea2ExpA
evalExpA (case Mul of
     Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
     Mul -> Prod (ea2ExpA e1) (ea2ExpA e2))
-- case of.1
evalExpA (Prod (ea2ExpA e1) (ea2ExpA e2))
-- evalExpA
evalExpA (ea2ExpA e1) * evalExpA (ea2ExpA e2)
-- hi1.1 y hi1.2
evalEA e1 * evalEA e2

CI2.d:
evalEA (BOp Mul e1 e2)
-- evalEV
case Mul of
    Sum -> evalEA e1 + evalEA e2
    Mul -> evalEA e1 * evalEA e2
-- case of.2
evalEA e1 * evalEA e2

ci2 demostrado.

-- b4
por ppio. de ext.:
para todo e:
    ¿evalEA . expA2ea e = evalExpA e?
-- (.)
    ¿evalEA (expA2ea e) = evalExpA e?

sea e' un elemento de tipo ExpA cualquiera,
quiero ver que:
    ¿evalEA (expA2ea e') = evalExpA e'?

por ppio. de ind. sobre la estructura ,
es eq. a demostrar:
    CB, e'=(Cte n):
        ¿evalEA (expA2ea (Cte n)) = evalExpA (Cte n)?
    CI1, e'=(Suma e1 e2):
        HI1: ¡evalEA (expA2ea e1) = evalExpA e1!
        HI2: ¡evalEA (expA2ea e2) = evalExpA e2!
        TI: ¿evalEA (expA2ea (Suma e1 e2)) = evalExpA (Suma e1 e2)?
    CI2, e'=(Prod e1 e2):
        HI1: ¡evalEA (expA2ea e1) = evalExpA e1!
        HI2: ¡evalEA (expA2ea e2) = evalExpA e2!
        TI: ¿evalEA (expA2ea (Prod e1 e2)) = evalExpA (Prod e1 e2)?

CB.i:
evalEA (expA2ea (Cte n))
-- expA2ea.1
evalEA (Const n)
-- evalEA.1
n

CB.d:
evalExpA (Cte n)
-- evalExpA.1
n

cb demostrado.

CI1.i:
evalEA (expA2ea (Suma e1 e2))
-- expA2ea.2
evalEA (BOp Sum (expA2ea e1) (expA2ea e2))
-- evalEA.2
case Sum of
    Sum -> evalEA (expA2ea e1) + evalEA (expA2ea e2)
    Mul -> evalEA (expA2ea e1) * evalEA (expA2ea e2)
-- case of.sum
evalEA (expA2ea e1) + evalEA (expA2ea e2)
-- hi1.1 y hi1.2
evalExpA e1 + evalExpA e2

CI1.d:
evalExpA (Suma e1 e2)
-- evalExpA.2
evalExpA e1 + evalExpA e2

ci1 demostrado.

CI1.i:
evalEA (expA2ea (Prod e1 e2))
-- expA2ea.2
evalEA (BOp Mul (expA2ea e1) (expA2ea e2))
-- evalEA.2
case Mul of
    Sum -> evalEA (expA2ea e1) + evalEA (expA2ea e2)
    Mul -> evalEA (expA2ea e1) * evalEA (expA2ea e2)
-- case of.mul
evalEA (expA2ea e1) * evalEA (expA2ea e2)
-- hi1.1 y hi1.2
evalExpA e1 * evalExpA e2

CI1.d:
evalExpA (Prod e1 e2)
-- evalExpA.3
evalExpA e1 * evalExpA e2

ci2 demostrado.

-- 2
-- a
-- b

-- 3
--a
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

-- b 
--1
Por ppio. de ext.:
para todo t.
    ¿heightT t = (length . ramaMasLarga) t?
--- (.)
    ¿heightT t = length (ramaMasLarga t)?

Sea t' un Tree cualquiera,
quiero ver que:
    ¿heightT t' = length (ramaMasLarga t')?

Por ppio. de ind. en la estructura de t',
es equivalente a demostar:
    Caso base, t'=EmptyT:
        ¿heightT EmptyT = length (ramaMasLarga EmptyT)?

I:
heightT EmptyT
-- (heighT)
0

D:
length (ramaMasLarga EmptyT)
-- (ramaMasLarga)
length []
-- (length)
0

Caso base demostrado.
   
    Caso ind, t'=(NodeT n t1 t2):
        HI1: ¡heightT t1 = length (ramaMasLarga t1)!
        HI2: ¡heightT t2 = length (ramaMasLarga t2)!
        TI: ¿heightT (NodeT n t1 t2) = length (ramaMasLarga (NodeT n t1 t2))?

I:
heighT (NodeT n t1 t2)
-- heighT
1 + max (heightT t1) (heightT t2)
-- hi.1
1 + max (length (ramaMasLarga t1)) (heightT t2)
-- hi.2
1 + max (length (ramaMasLarga t1)) length (ramaMasLarga t2)
-- max
1 + (if (length (ramaMasLarga t1) >= length (ramaMasLarga t2)
        then length (ramaMasLarga t1)
        else length (ramaMasLarga t2))

D:
length (ramaMasLarga (NodeT n t1 t2))
-- (ramaMasLarga)
length (x : laDeMayorLongitud (ramaMasLarga t1) (ramaMasLarga t2))
-- (laDeMayorLongitud)
length (x : (if length (ramaMasLarga t1) >= length (ramaMasLarga t2)
                           then ramaMasLarga t1
                           else ramaMasLarga t2))
-- length
1 + length (if length (ramaMasLarga t1) > length (ramaMasLarga t2)
                then ramaMasLarga t1
                else ramaMasLarga t2)
-- lema.1
1 + (if (length (ramaMasLarga t1) >= length (ramaMasLarga t2)
        then length (ramaMasLarga t1)
        else length (ramaMasLarga t2))

Caso ind. demostrado.

-- lema 1
length (if b
    then xs
    else ys) =
if b
    then length xs
    else length ys

Por analisis de caso sobre b,
    C1: b = True
    ¿length (if True then xs else ys)
        = if True then length xs else length ys?
    C2: b = False
    ¿length (if False then xs else ys)
        = if False then length xs else length ys?

C1.i:
length (if True then xs else ys)
-- if-then-else, True
length xs

C1.d:
if True then length xs else length ys
-- if-then-else, True
length xs

el caso True se cumple.

C2.i:
length (if False then xs else ys)
-- if-then-else, False
length ys

C2.d:
if False then length xs else length ys
-- if-then-else, False
length ys

el caso False se cumple.

--2
Por ppio. de ext.:
    ¿para todo t. (reverse . inOrder) t = (inOrder . mirrorT) t?
                                                                  (def (.))
    ¿para todo t. reverse (inOrder t) = inOrder (mirrorT t)?

Sea t' unTree cualquiera, quiero ver que:

Por ppio. de induccion sobre la estructura de t':
    Caso base, t'=EmptyT: ¿reverse (inOrder EmptyT) = inOrder (mirrorT EmptyT)?

Izq:
    reverse (inOrder EmptyT)
= (inOrder)
    reverse []
= (reverse)
    []

Der:
    inOrder (mirrorT EmptyT)
= (mirrorT)
    inOrder EmptyT
= (inOrder)
    []

Caso base demostrado.

    Caso ind, t'=(NodeT n t1 t2):
      H1: ¡reverse (inOrder t1) = inOrder (mirrorT t1)!
      H2: ¡reverse (inOrder t2) = inOrder (mirrorT t2)!
      TI: ¿reverse (inOrder (NodeT n t1 t2)) = inOrder (mirrorT (NodeT n t1 t2))?

Izq:
    reverse (inOrder (NodeT n t1 t2))
= (inOrder.2)
    reverse (inOrder t1 ++ [n] ++ inOrder t2)
= (Asoc.)
    reverse (inOrder t1 ++ ([n] ++ inOrder t2))
= (LEMA 1)
    reverse ([n] ++ inOrder t2) ++ reverse (inOrder t1)
= (LEMA 1)
    (reverse (inOrder t2) ++ reverse [n]) ++ reverse (inOrder t1)
= (Asoc.)
    reverse (inOrder t2) ++ reverse [n] ++ reverse (inOrder t1)
= (reverse.2)
    reverse (inOrder t2) ++ (reverse [] ++ [n]) ++ reverse (inOrder t1)
= (reverse.1)
    reverse (inOrder t2) ++ ([] ++ [n]) ++ reverse (inOrder t1)
= (++)
    reverse (inOrder t2) ++ [n] ++ reverse (inOrder t1)

Der:
    inOrder (mirrorT (NodeT n t1 t2))
= (mirrorT.2)
    inOrder (NodeT n (mirrorT t2) (mirrorT t1)) 
= (inOrder.2)
    inOrder (mirrorT t2) ++ [n] ++ inOrder (mirrorT t1)
= (H1, H2)
    reverse (inOrder t2) ++ [n] ++ reverse (inOrder t1)

Por ppio. de ext.:
para todo xs. para todo ys.
 ¿reverse (xs ++ ys) = reverse ys ++ reverse xs?

Sean zs y ws listas cualquiera (finitas y totalmente definifas),
se demostrara que:
    ¿reverse (zs ++ ws) = reverse ws ++ reverse zs?

Por ppio. de ind. sobre la estructura de zs,
es equivalente a demostar:
    Caso base, zs=[]:
         ¿reverse ([] ++ ws) = reverse ws ++ reverse []?

I:
reverse ([] ++ ws)
-- = (++)
reverse ws

D:
  reverse ws ++ reverse []
-- = (reverse.1)
  reverse ws ++ []
-- = (++)
  reverse ws

Caso base demostrado.

    Caso ind., zs=(z:zs'):
        H1: ¡reverse (zs' ++ ws) = reverse ws ++ reverse zs'!
        TI: ¿reverse ((z:zs') ++ ws) = reverse ws ++ reverse (z:zs')?

I:
  reverse ((z:zs') ++ ws)
-- = (++) 
  reverse (z : (zs' ++ ws))
-- = (reverse)
  reverse (zs' ++ ws) ++ [z]
-- = (HI)
  (reverse ws ++ reverse zs') ++ [z]
-- = (())
  reverse ws ++ reverse zs' ++ [z]

D:
  reverse ws ++ reverse (z:zs')
-- = (reverse)
  reverse ws ++ (reverse zs' ++ [z])
-- = (())
  reverse ws ++ reverse zs' ++ [z]

Caso ind. demostrado.

-- 4

-- 5

-- a1
por ppio. de ext.:
para todo d:
    ¿(objs . excavar) d = (doblar . objs) d?
-- (.)
    ¿objs (excavar d) = doblar (objs d)?

sea d' un elemento de tipo Dungeon cualquiera,
quiero ver que:
    ¿objs (excavar d') = doblar (objs d')?

por ppio. de ind. sobre la estructura d',
es eq. a demostrar:
    CB, d'=Cueva:
        ¿objs (excavar Cueva) = doblar (objs Cueva)?
    CI, d'=(Habitacion os d1 d2):
        HI1: ¡objs (excavar d1) = doblar (objs d1)!
        HI2: ¡objs (excavar d2) = doblar (objs d2)!
        TI: ¿objs (excavar (Habitacion os d1 d2)) = doblar (objs (Habitacion os d1 d2))?

CB.i:
objs (excavar Cueva)
-- excavar.1
objs (Habitacion [] Cueva Cueva)
-- objs.1
[] ++ objs Cueva ++ objs Cueva
-- objs.1
[] ++ [] ++ []
-- ++
[]

CB.d:
doblar (objs Cueva)
-- objs.1
doblar []
-- doblar.1
[]

cb demostrado.

CI.i:
objs (excavar (Habitacion os d1 d2))
-- excavar
objs (Habitacion (doblar os) (excavar d1) (excavar d2))
-- objs
(doblar os) ++ objs (excavar d1) ++ objs (excavar d2)
-- hi.1 y hi.2
(doblar os) ++ (doblar (objs d1)) ++ (doblar (objs d2))

CI.d:
doblar (objs (Habitacion os d1 d2))
-- objs
doblar (os ++ objs d1 ++ objs d2)
-- asoc. ++
doblar (os ++ (objs d1 ++ objs d2)))
-- lema.1
doblar os ++ doblar (objs d1 ++ objs d2)
-- lema.1
doblar os ++ (doblar (objs d1) ++ doblar (objs d2))
-- asoc. ++
(doblar os) ++ (doblar (objs d1)) ++ (doblar (objs d2))

ci demostrado.

-- lema 1
doblar (as ++ bs) = doblar as ++ doblar bs
    CB, as []:
        ¿doblar ([] ++ bs) = doblar [] ++ doblar bs?
    CI, as = (a:as'):
        HI: ¡doblar (as' ++ bs) = doblar as' ++ doblar bs!
        TI: ¿doblar ((a:as') ++ bs) = doblar (a:as') ++ doblar bs?

cb.i:
doblar ([] ++ bs)
-- ++
doblar bs

cb.d:
doblar [] ++ doblar bs
-- doblar.1
[] ++ doblar bs
-- ++
doblar bs

cb demostado.

ci.i:
doblar ((a:as') ++ bs)
-- ++
doblar (a : (as' ++ bs))
-- doblar.2
doblarObj a ++ doblar (as' ++ bs)
-- hi
doblarObj a ++ doblar as' ++ doblar bs
-- asoc. ++
(doblarObj a ++ doblar as') ++ doblar bs

ci.d:
doblar (a:as') ++ doblar bs
-- doblar.2
(doblarObj a ++ doblar as') ++ doblar bs

ci demostado.

-- a2
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