-- 1
-- a
first :: (a,b) -> a
(x,y) :: (a,b)
first (x,y) :: a

x :: a
y :: b
(x,y) :: (a,b)

-- b
apply :: (a -> b) -> (a -> b)
f :: a -> b
apply f :: a -> b

g :: a -> b
x :: a
g x :: b

f :: a -> b
x :: a
f x :: b

-- c
twice :: (a -> a) -> (a -> a)
f :: a -> a
twice f :: a -> a

g :: a -> a
x :: a
g x :: a

f :: a -> a
f x :: a
f (f x) :: a 

f :: a -> a
x :: a
f x :: a

-- d
doble :: Int -> Int
x :: Int
doble x :: Int

x :: Int
x :: Int
x + x :: Int

-- e
swap :: (a,b) -> (b,a)
(x,y) :: (a,b)
swap (x,y) :: (b,a)

x :: a
y :: b
(x,y) :: (a,b)

-- f
unflip :: ((b,a) -> c) -> ((a,b) -> c)
f :: (b,a) -> c
unflip f :: (a,b) -> c

g :: (a,b) -> c
p :: (a,b)
g p :: c

f :: (b,a) -> c
swap p :: (b,a)
f (swap p) :: c

swap :: (a,b) -> (b,a)
p :: (a,b)
swap p :: (b,a)

-- 2
-- a
apply :: (a -> b) -> (a -> b)
first :: (c,d) -> c
-- (a <- (c,d), b <- c)
apply first :: (c,d) -> c

-- b
first :: -> (a,b) -> a
(swap, uflip) :: ((c,d) -> (d,c), ((d,c) -> e) -> ((c,d) -> e))
-- (a <- (c,d) -> (d,c), b <- ((d,c) -> e) -> ((c,d) -> e))
first (swap, uflip) :: (c,d) -> (d,c)

-- c
twice :: (a -> a) -> (a -> a)
doble :: Int -> Int
-- (a <- Int)
twice doble :: Int -> Int

-- d
twice :: (a -> a) -> (a -> a)
twice :: (b -> b) -> (b -> b)
-- (a <- (b -> b))
twice twice :: (b -> b) -> (b -> b)

-- e
twice :: (a -> a) -> (a -> a) 
uflip :: ((c,b) -> d) -> ((b,c) -> d)
-- (c <- b, a <- ((b,b) -> d))
twice uflip :: ((b,b) -> d) -> ((b,c) -> d)

-- f
twice :: (a -> a) -> (a -> a) 
swap :: (b,c) -> (c,b)
-- (c <- b, a <- (b,b))
twice swap :: (b,b) -> (b,b)

-- g
uflip :: ((b,a) -> c) -> ((a,b) -> c)
swap :: (d,e) -> (e,d)
-- (b <- d, a <- e, c <- (e,d))
uflip swap :: (e,d) -> (e,d)

-- h
twice twice :: (a -> a) -> (a -> a)
swap :: (b,c) -> (c,b)
-- (c <- b, a <- (b,b))
(twice twice) swap :: (b,b) -> (b,b)

-- 3
const :: a -> b -> a -- vii
const x y = x

appDup :: ((a,a) -> b) -> a -> b -- ii
appDup f x = f (x, x)

appFork :: (a -> b, a -> c) -> a -> (b,c) -- v
appFork (f,g) x = (f x, g x)

appPar :: (a -> b, c -> d) -> (a,c) -> (b,d) -- i
appPar (f,g) (x,y) = (f x, g y)

appDist :: (a -> b) -> (a,a) -> (b,b) -- iv
appDist f (x,y) = (f x, f y)

flip :: (a -> b -> c) -> b -> a -> c -- iii
flip f x y = f y x

subst :: (a -> b -> c) -> (a -> b) -> a -> c -- vi
subst f g x = f x (g x)

-- 4
1 && 2 == 2 -- No posee tipo
1 + if 3 < 5 then 3 else 5 -- :: Int
let par = (True, 4)
        in (if first par then first par else second par) -- No posee tipo
(doble doble) 5 -- No posee tipo
doble (doble 5) -- :: Int
twice first -- No posee tipo
(twice doble) doble -- No posee tipo
(twice twice) first -- No posee tipo
apply apply -- :: (a -> b) -> (a -> b)

-- 5
-- a
True
False

-- b
(1,2)
(3,4)

-- c
posicionEnAbecedario
posicionEnVocales

-- d
esPosicionEnAbecedario
esPosicionEnVocales

-- e
\f -> f 10
\f -> f 5

-- f
(esTrue, 1)
(esFalse, 2)

-- g
\x -> True
\x -> False

-- 6
-- a
\p -> let (f, g) = p
        in \x -> (f x, g x) -- v

-- b
\f -> (\g -> (\x -> f x (g x)) -- vi

-- c
\f -> (\x -> (\y -> (f y) x) -- iii

-- d
\f -> (\px -> let (x, y) = px
                in (f x, f y)) -- iv

-- e
\x -> (\y -> x) -- vii

-- f
 \pf -> let (f, g) = pf
            in \px -> let (x, y) = px
                        in (f x, g y) -- i

-- g
\f -> (\x -> f (x, x)) -- ii

-- 7
appFork (id,id) = \x -> (x, x)
\f -> appDup (appDist f) = \f -> \x -> (f x, f x)
appDup id = \x -> (x,x)
appDup appFork -- No tiene tipo válido
flip (appDup const) = \x -> \y -> (y, y)
const (appDup id) = \y -> \x -> (x,x)

-- a1
yTambien :: Bool -> Bool -> Bool

-- a2
-- a
doble :: Int -> Int
x :: Int
doble x :: Int

x :: Int
x :: Int
x + x :: Int

-- b
id :: a -> a
x :: a
id x :: a

-- c
apply :: (a -> b) -> (a -> b)
f :: a -> b
apply f :: a -> b 
 
g :: a -> b
x :: a
g x :: b

f :: a -> b
x :: a
f x :: b

-- d
suma x :: Int -> Int
y :: Int 
suma x y :: Int

suma :: Int -> Int -> Int 
x :: Int
suma x :: Int -> Int

x :: Int
y :: Int
x + y :: Int

-- e
const :: a -> (b -> a)
x :: a
const x :: b -> a

g :: b -> a
y :: b
g y :: a

-- f
compose :: (b -> c) -> ((a -> b) -> (a -> c))
f :: b -> c
compose f :: (a -> b) -> (a -> c)

h :: (a -> b) -> (a -> c)
g :: a -> b
h g :: a -> c

k :: a -> c
x :: a 
k x :: c

f :: b -> c
g x :: b
f (g x) :: c

g :: a -> b
x :: a
g x :: b

-- g
flip :: a -> (b -> c) -> (b -> (a -> c))
f :: a -> (b -> c)
flip f :: b -> (a -> c)

g :: b -> (a -> c)
x :: b
g x :: a -> c

h :: a -> c
y :: a
h y :: c

f y :: b -> c
x :: b
(f y) x :: c

f :: a -> (b -> c)
y :: a
f y :: b -> c

-- h
subst :: (a -> (b -> c)) -> ((a -> b) -> (a -> c))
f :: a -> (b -> c)
subst f :: (a -> b) -> a -> c

h :: (a -> b) -> (a -> c)
g :: a -> b
h g :: a -> c

k :: a -> c
x :: a
k x :: c

f x :: b -> c
g x :: b
(f x) (g x) :: c

f :: a -> (b -> c)
x :: a
f x :: b -> c

g :: a -> b
x :: a
g x :: b