-- 1
-- a
evalEA :: EA -> Int
evalEA (Const n) = n
evalEA (BOp binop e1 e2) = case binop of
    Sum -> (evalEA e1) + (evalEA e2)
    Prod -> (evalEA e1) * (evalEA e2)

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp binop e1 e2) = case binop of
     Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
     Mul -> Prode (ea2ExpA e1) (ea2ExpA e2)

expA2ea :: ExpA -> EA
expA2ea (Cte n) = Const n
expA2ea (Suma e1 e2) = BOp Sum (ea2ExpA e1) (ea2ExpA e2)
expA2ea (Prod e1 e2) = BOp Prod (ea2ExpA e1) (ea2ExpA e2)

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
laDeMayorLongitud xs ys = if longitud xs > longitud ys
                           then xs
                           else ys

todosLosCaminos :: Tree a -> [[a]]

-- b 
--1
Por ppio. de ext.:
para todo t.
    ¿heightT t = (length . ramaMasLarga) t?
--- (.)
    ¿heightT t = length (ramaMasLarga t)?

Sea t' un Tree cualquiera (finito y totalmente definido),
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
   
    Caso ind,  t'=(NodeT n t1 t2):
        HI1: ¡heightT t1 = length (ramaMasLarga t1)!
        HI2: ¡heightT t2 = length (ramaMasLarga t2)!
        TI: ¿heightT (NodeT n t1 t2) = length (ramaMasLarga (NodeT n t1 t2))?

I:
heighT (NodeT n t1 t2)
-- heighT
1 + max (heightT t1) (heightT t2)

D:
length (ramaMasLarga (NodeT n t1 t2))
-- (ramaMasLarga)
length (x : laDeMayorLongitud (ramaMasLarga t1) (ramaMasLarga t2))
-- (laDeMayorLongitud)
length (x :(if length (ramaMasLarga t1) > length (ramaMasLarga t2)
                           then ramaMasLarga t1
                           else ramaMasLarga t2))

Caso ind. demostrado.

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
=                           (inOrder)
    reverse []
=                           (reverse)
    []

Der:
    inOrder (mirrorT EmptyT)
=                           (mirrorT)
    inOrder EmptyT
=                           (inOrder)
    []

Caso base demostrado.

    Caso ind, t'=(NodeT n t1 t2):
      H1: ¡reverse (inOrder t1) = inOrder (mirrorT t1)!
      H2: ¡reverse (inOrder t2) = inOrder (mirrorT t2)!
      TI: ¿reverse (inOrder (NodeT n t1 t2)) = inOrder (mirrorT (NodeT n t1 t2))?

Izq:
    reverse (inOrder (NodeT n t1 t2))
=                                                               (inOrder.2)
    reverse (inOrder t1 ++ [n] ++ inOrder t2)
=                                                               (Asoc.)
    reverse (inOrder t1 ++ ([n] ++ inOrder t2))
=                                                               (LEMA 1)
    reverse ([n] ++ inOrder t2) ++ reverse (inOrder t1)
=                                                               (LEMA 1)
    (reverse (inOrder t2) ++ reverse [n]) ++ reverse (inOrder t1)
=                                                               (Asoc.)
    reverse (inOrder t2) ++ reverse [n] ++ reverse (inOrder t1)
=                                                               (reverse.2)
    reverse (inOrder t2) ++ (reverse [] ++ [n]) ++ reverse (inOrder t1)
=
    reverse (inOrder t2) ++ ([] ++ [n]) ++ reverse (inOrder t1)
=
    reverse (inOrder t2) ++ [n] ++ reverse (inOrder t1)

Der:
    inOrder (mirrorT (NodeT n t1 t2))
=                                                       (mirrorT.2)
    inOrder (NodeT n (mirrorT t2) (mirrorT t1)) 
=                                                       (inOrder.2)
    inOrder (mirrorT t2) ++ [n] ++ inOrder (mirrorT t1)
=                                                       (H1, H2)
    reverse (inOrder t2) ++ [n] ++ reverse (inOrder t1)

LEMA 1: reverse (xs ++ ys) = reverse ys ++ reverse xs
Sean zs y ws listas, por ppio de ind. sobre la estructura de zs, se verá que:
    Caso base, zs=[]: ¿reverse ([] ++ ws) = reverse ws ++ reverse []?

Izq:
    reverse ([] ++ ws)
=                       (++)
    reverse ws

Der:
  reverse ws ++ reverse []
=                          (reverse.1)
  reverse ws ++ []
=                          (++)
  reverse ws

Caso base demostrado.

Caso ind., zs=(z:zs'):
    H1: ¡reverse (zs' ++ ws) = reverse ws ++ reverse zs'!
    TI: ¿reverse ((z:zs') ++ ws) = reverse ws ++ reverse (z:zs')?

Izq:
  reverse ((z:zs') ++ ws)
=
  reverse (z : (zs' ++ ws))
=
  reverse (zs' ++ ws) ++ [z]
=
  (reverse ws ++ reverse zs') ++ [z]
=
  reverse ws ++ reverse zs' ++ [z]

Der:
  reverse ws ++ reverse (z:zs')
=
  reverse ws ++ (reverse zs' ++ [z])
=
  reverse ws ++ (reverse zs' ++ [z])
=
  reverse ws ++ reverse zs' ++ [z]

Caso ind. demostrado.

-- 4

-- 5

-- a1
-- a2