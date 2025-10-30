-- S1
-- 1
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
(++) (x:xs) ys = x : (++) xs ys

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

-- 2
-- a
por ppio. de extencionalidad:
para todo xs, ys.
    ¿length (xs ++ ys) = length xs + length ys?

Sean as, bs listas cualquiera finitas y totalmente definidas,
quiero demostrar que:
    ¿length (as ++ bs) = length as + length bs?

por ppio. de induccion estrucrutal sobre as,
es equivalente a demostar:
    CB, as=[]:
        ¿length ([] ++ bs) = length [] + length bs?

I:
length ([] ++ bs)
-- = (++)
length bs

D:
length [] + length bs
-- = (length)
0 + length bs
-- = (aritm.)
length bs

    CB demostrado.
    CI, as=(a:as')
        HI: ¡length (as' ++ bs) = length as' + length bs!
        TI: ¿length ((a:as') ++ bs) = length (a:as') + length bs?

I:
length ((a:as') ++ bs)
-- (++)
length (a : (as' ++ bs))
-- (length)
1 + length (as' ++ bs) 
-- (H1)
1 + length as' + length bs

D:
length (a:as') + length bs  
-- (length)
(1 + length as') + length bs
-- (asoc. de ())
1 + length as' + length bs

-- b
Por ppio. de ext.:
para todo xs.
    ¿count (const True) xs = length xs?

Sea as una lista cualquiera finita y totalmente definida,
quiero demostrar que:
    ¿count (const True) as = length as?

Por ppio. de ind. en la estructura de as,
es equivalente a demostar:
    Caso base, as=[]:
        ¿count (const True) [] = length []?

I:
count (const True) []
-- (count.1)
0

D:
length []
-- (length.1)
0

Caso base demostrado.
    Caso ind.1,  as=(b:bs) :
        HI: ¡count (const True) bs = length bs!
        TI: ¿count (const True) (b:bs) = length (b:bs)?

I:
count (const True) (b:bs)
-- (count.2)
if const True b
    then 1 + count (const True) bs
    else count (const True) bs
-- (HI)
if const True b
    then 1 + length bs
    else length bs
-- (const)
if True
    then 1 + length bs
    else length bs
-- (eval. if)
1 + length bs


D:
length (b:bs)
-- (length)
1 + length bs

Caso ind. demostrado.

-- 3
Por ppio. de ext.:
para todo x.
    ¿elem x = any . (==) x?
para todo x,ys.
    ¿elem x ys = any ((==) x) ys?
-- (.)
    ¿elem x ys = any ((==) x) ys?

Sean a y bs un elemento y una lista respectivamente finitos y totalmente definidos,
quiero demostrar que:
    ¿elem a bs = any ((==) a) bs?

Por ppio. de ind. en la estructura de bs,
es equivalente a demostar:
    Caso base, bs=[]:
        ¿elem a [] = any ((==) a) []?

I:
elem a []
-- (elem.1)
False

D:
any ((==) a) []
-- (any.1)
False

Caso base demostrado.
    
    Caso ind, bs=(c:cs):
        HI: ¡elem a cs = any ((==) a) cs!
        TI: ¿elem a (c:cs) = any ((==) a) (c:cs)?

I:
elem a (c:cs)
-- (elem.2)
a == c || elem a cs

D:
any ((==) a) (c:cs)
-- (any)
((==) a) c || any (==) a) cs
-- (HI)
((==) a) c || elem a cs
-- (==)
a == c || elem a cs

Caso ind. demostrado.

-- 4
para todo x.
    ¿any (elem x) = elem x . concat?
Por ppio. de ext.:
para todo x,yss.
    ¿any (elem x) yss = (elem x . concat) yss?
-- (.)
    ¿any (elem x) yss = elem x (concat yss)?

Sean a y bss un elemento y una lista de listas respectivamente cualquiera, finitos y totalmente definidos,
quiero demostrar que:
    ¿any (elem a) bss = elem a (concat bss)?

Por ppio. de ind. en la estructura de as,
es equivalente a demostar:
    Caso base, bss=[]:
        ¿any (elem a) [] = elem a (concat [])?

I:
any (elem a) []
-- (any.1)
False

D:
elem a (concat [])
-- (concat.1)
elem a []
-- (elem.1)
False

Caso base demostrado.
   
    Caso ind, bss=(cs:css):
        HI: ¡any (elem a) css = elem a (concat css)!
        TI: ¿any (elem a) (cs:css) = elem a (concat (cs:css))?

I:
any (elem a) (cs:css)
-- (any)
elem a cs || any (elem a) css
-- (HI)
elem a cs || elem a (concat css)

D:
elem a (concat (cs:css))
-- (concat)
elem a (cs ++ concat css)
-- (Lema.1)
elem a cs || elem a (concat css)

-- Lema.1
elem a (xs ++ ys) = elem a xs || elem a ys

Sea e un elemento cualquiera y ws e zs dos listas cualquiera (finitas y totalmente definidas) del mismo tipo de e.

Por ppio. de ind. en la estructura de ws,
es equivalente a demostar:
    Caso base, ws=[]:
        ¿elem e ([] ++ zs) = elem e [] || elem a zs?

I:
elem e ([] ++ zs)
-- (++.1)
elem e zs

D:
elem e [] || elem a zs
-- (elem.1)
False || elem a zs
-- (||)
elem a zs

Caso base demostrado.

Caso ind, ws=(w:ws'):
        HI: ¡elem e (ws' ++ zs) = elem e ws' || elem a zs!
        TI: ¿elem e ((w:ws') ++ zs) = elem e (w:ws') || elem a zs?

I:
elem e ((w:ws') ++ zs)
-- (++.2)
elem e (w : (ws' ++ zs))
-- (elem.2)
e == w || elem e (ws' ++ zs)
-- (HI)
e == w || elem e ws' || elem a zs

D:
elem e (w:ws') || elem a zs
-- (elem.2)
e == w || elem e ws' || elem a zs
-- (HI)

Caso ind. demostrado.

-- e
para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs

-- f
all null = null . concat

-- g
length = length . reverse

-- h
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

-- i
para todo xs. para todo ys.
all p (xs++ys) = all p (reverse xs) && all p (reverse ys)

-- j
para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)

-- S2
-- 1
-- a
evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z m = m
addN (S n) m = S (addN n m)

prodN :: N -> N -> N
prodN Z _ = Z
prodN _ Z = Z
prodN (S n) m = addN (prodN n m) m

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

-- b
-- 1
para todo n1, n2.
    ¿evalN (addN n1 n2) = evalN n1 + evalN n2?

Sean n1'::N, n2'::N. Qvq:
-- Elementos cualquiera totalmente definidos y finitos de N.

Por ppio. de induccion sobre la estructura de n1':
    Caso base, n1' = Z:
        ¿evalN (addN Z n2') = evalN Z + evalN n2'?
    Caso ind, n1' = S n: 
        H1: ¡evalN (addN n n2') = evalN n + evalN n2'!
        Tesis: ¿evalN (addN (S n) n2') = evalN (S n) + evalN n2'?

Caso base:
Izq:
    evalN (addN Z n2')
-- = (addN.1)
    evalN n2'

Der: 
    evalN Z + evalN n2'
-- = (evalN.2)
    0 + evalN n2'
-- = (Aritm.)
    evalN n2'

Caso base demostrado.

Caso ind:
Izq:
    evalN (addN (S n) n2')
-- = (addN.2)   
    evalN (S (addN n n2'))
-- = (evalN.2)
    1 + evalN (addN n n2')
-- = (HI)
    1 + evalN n + evalN n2'

Der:
    evalN (S n) + evalN n2'
-- = (evalN.2)
    1 + evalN n + evalN n2'

Caso ind demostrado.

-- 2
¿para todo n1. para todo n2.
    evalN (prodN n1 n2) = evalN n1 * evalN n2?

Sea n1'::Z, n2'::Z, Qvq:

Por ppio. de induccion sobre la estructura de n1':
    Caso base, n1' = Z:
        ¿evalN (prodN Z n2') = evalN Z * evalN n2'?
    Caso ind, n1' = S n: 
        H1: ¡evalN (prodN n n2') = evalN n * evalN n2'!
        Tesis: ¿evalN (prodN (S n) n2') = evalN (S n) * evalN n2'?

Caso base:
Izq:
    evalN (prodN Z n2')
-- = (prodN.1)
    evalN Z
-- = (evalN.1)
    0

Der:
    evalN Z * evalN n2'
-- = (evalN.1)
    0 * evalN n2'
-- = (Aritm.)
    0      

Caso base demostrado.

Caso ind:
Izq:
    evalN (prodN (S n) n2')
-- = (prodN.2)
    evalN (addN (prodN n n2') n2')
-- = (Lema1)
    evalN (prodN n n2') + evalN n2'
-- = (H1)
    (evalN n * evalN n2') + evalN n2'

Der:
    evalN (S n) * evalN n2'
-- = (evalN)
    (1 + evalN n) * eval n2'
-- = (Aritm. distributiva)
    (1 * eval n2') + (evalN n * eval n2')
-- = (Aritm. 1 neutro)
    eval n2' + (evalN n * eval n2')
-- = (Aritm. conmutatividad)
    (evalN n * evalN n2') + evalN n2'

-- Lema 1
evalN (addN a b) = evalN a + evalN b
Sean n1::N, n2::N. Qvq:

Por induccion estructural sobre n1, se vera que:
    evalN (addN n1 n2) = evalN n1 + evalN n2

Caso base, n1 = Z:
    ¿evalN (addN Z n2) = evalN Z + evalN n2?
Caso ind, n1 = S n:
    HI: ¡evalN (addN n n2) = evalN n + evalN n2!
    TI: ¿evalN (addN (S n) n2) = evalN (S n) + evalN n2?

Caso base:
Izq:
    evalN (addN Z n2)
-- = (addN.1)
    eval n2

Der:
    evalN Z + evalN n2
-- = (evalN.1)
    0 + evalN n2
-- = (Aritm.)
    evalN n2

Caso ind:
Izq:
    evalN (addN (S n) n2)
-- = (addN.2)
    evalN (S (addN n n2))
-- = (evalN.2)
    1 + evalN (addN n n2)

Der:
    evalN (S n) + evalN n2
-- = (evalN.2)
    1 + evalN n + evalN n2
-- = (HI)
    1 + evalN (addN n n2)

-- 3
Por ppio. de ext.:
para todo x.
    ¿(int2N . evalN) x = id x?
-- (.)
    ¿int2N (evalN x) = id x?

Sea n::N. Qvq:

Por ind. estr. sobre n:
    ¿int2N (evalN n) = id n?

Caso base, n=Z:
    ¿int2N (evalN Z) = id Z?
Caso ind, n=(S n')
    H1: ¡int2N (evalN n') = id n'!
    TI: ¿int2N (evalN (S n')) = id (S n')?

CB:
I:
    int2N (evalN Z)
-- = (evalN.1)
    int2N 0
-- = (int2N.1)
    Z

D:
    id Z
-- = (id)
    Z

CI:
I:
    int2N (evalN (S n'))
-- = (evalN)
    int2N (1 + evalN n')
-- = (int2N)
    S (int2N ((1 + evalN n')-1))
-- = (Aritm.)
    S (int2N (evalN n'))
-- = (HI)
    S n'

D:
    id (S n')
-- = (id)
    S n'

-- 4
Por ppio. de ext.:
para todo x.
    ¿(evalN . int2N) x = id x?
-- = (.)
    ¿evalN (int2N x) = id x?

Sea n::N. Qvq:

Por ind. matematica sobre n:
    ¿evalN (int2N n) = id n?

Caso base, n=0:
    ¿evalN (int2N 0) = id 0?
Caso ind, n > 0:
    H1: ¡evalN (int2N (n - 1)) = id (n - 1)!
    TI: ¿evalN (int2N n) = id n?

CB:
I:
    evalN (int2N 0)
-- = (int2N)
    evalN Z
-- = (evalN)
    0

D:
    id 0
-- = (id)
    0

CI:
I:
    evalN (int2N n)
-- = (int2N)
    evalN (S (int2N (n-1)))
-- = (evalN)
    1 + evalN (int2N (n-1))
-- = (HI)
    1 + id (n - 1)
-- = (id)
    1 + (n - 1)
-- = (aritm.)
    n

D:
    id n
-- (id)
    n
-- S3
-- 1
-- 2