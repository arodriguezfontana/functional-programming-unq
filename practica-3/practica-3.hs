-- 1
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

-- 2
-- a
apply f = g where g x = f x
apply f = g where g = \x -> f x
apply f = \x -> f x 
apply = \f -> (\x -> f x)
(apply f) x = f x

-- b
twice f = g where g x = f (f x)
twice f = g where g = \x -> f (f x)
twice f = \x -> f (f x)
twice = \f -> (\x -> f (f x))
(twice f) x = f (f x)

-- c
id = \x -> x
id x = x

-- d
flip f = g where g x = h where h y = (f y) x
flip f = g where g x = h where h = \y -> (f y) x
flip f = g where g = \x -> (\y -> (f y) x)
flip f = \x -> (\y -> (f y) x)
flip = \f (\x -> (\y -> (f y) x))
(flip f) x y = (f y) x

-- e
unflip f = g where g p = f (swap p)
unflip f = g where g = \p -> f (swap p)
unflip f = \p -> f (swap p)
unflip = \f -> (\p -> f (swap p))
(unflip f) p = f (swap p)

-- f
const = \x -> (\y ->x)
(const x) y = x

-- g
compose = \f -> (\g -> (\x -> f (g x)))
((compose f) g) x = f (g x)

-- 3
-- a
apply :: (a -> b) -> (a -> b)
f :: a -> b
------------------
apply f :: a -> b

apply f :: a -> b
x :: a
-----------------
(apply f) x :: b

f :: a -> b
x :: a
---------
f x :: b

-- b
twice :: (a -> a) -> (a -> a)
f :: a -> a
------------------
twice f :: a -> a

twice f :: a -> a
x :: a
-----------------
(twice f) x :: a

f :: a -> a
f x :: a
-------------
f (f x) :: a

f :: a -> a
x :: a
---------
f x :: a

-- c
id :: a -> a
x :: a
----------
id x :: a

-- d
flip :: (a -> (b -> c)) -> (b -> (a -> c))
f :: a -> (b -> c)
-----------------------
flip f :: b -> (a -> c)

flip f :: b -> (a -> c)
x :: b
---------------------
(flip f) x :: a -> c

(flip f) x :: a -> c
y :: a
------------------
(flip f) x y :: c

f :: a -> (b -> c)
y :: a
--------------
f y :: b -> c

f y :: b -> c
x :: b
------------
(f y) x :: c

-- e
unflip :: ((b,a) -> c) -> ((a,b) -> c)
f :: (b,a) -> c
----------------------
unflip f :: (a,b) -> c

unflip f :: (a,b) -> c
p :: (a,b)
-----------------
(unflip f) p :: c

f :: (b,a) -> c
swap p :: (b,a)
---------------
f (swap p) :: c

swap :: (a,b) -> (b,a)
p :: (a,b)
---------------
swap p :: (b,a)

-- f
const :: a -> (b -> a)
x :: a
------------------
const x :: b -> a

const x :: b -> a
y :: b
----------------
(const x) y :: a

--g
compose :: (b -> c) -> ((a -> b) -> (a -> c))
f :: b -> c
---------------------------------
compose f :: (a -> b) -> (a -> c)

compose f :: (a -> b) -> (a -> c)
g :: a ->  b
-----------------------
(compose f) g :: a -> c

(compose f) g :: a -> c
x :: a
----------------------
((compose f) g) x :: c

f :: b -> c
g x :: b
-------------
f (g x) :: c

g :: a -> b
x :: a
---------
g x :: b

-- 4
-- a
apply :: (a -> b) -> (a -> b)
apply :: (c -> d) -> (c -> d)
----------------------------------- (a <- (c -> d), b <- (c -> b))
apply apply :: (c -> d) -> (c -> d)

apply apply :: (c -> d) -> (c -> d)
apply :: (e -> f) -> (e -> f)
------------------------------------------- (c <- (e -> f), d <- (e -> f))
(apply apply) apply :: (e -> f) -> (e -> f)

-- b
twice :: (a -> a) -> (a -> a)
doble :: Int -> Int
-------------------------- (a <- Int)
twice doble :: Int -> Int

twice doble :: Int -> Int
2 :: Int
-----------------------
(twice doble) 2 :: Int

-- c
twice :: (a -> a) -> (a -> a)
twice :: (b -> b) -> (b -> b)
----------------------------------- (a <- (b -> b))
twice twice :: (b -> b) -> (b -> b)

twice twice :: (b -> b) -> (b -> b)
twice :: (c -> c) -> (c -> c)
------------------------------------------- (b <- (c -> c))
(twice twice) twice :: (c -> c) -> (c -> c)

(twice twice) twice :: (c -> c) -> (c -> c)
swap :: (d,e) -> (e,d)
-------------------------------------------- (d <- e, c <- (e,e))
((twice twice) twice) swap :: (e,e) -> (e,e)

-- d
flip :: (a -> (b -> c)) -> (b -> ((d -> d) -> c))
twice :: (d -> d) -> (d -> d)
---------------------------------- ((b -> c) <- a, a <- (d -> d))
flip twice :: b -> ((d -> d) -> c)

flip twice :: b -> ((d -> d) -> c)
1 :: Int
---------------------------------- (b <- int)
(flip twice) 1 :: (d -> d) -> c

(flip twice) 1 :: (d -> d) -> c
doble :: Int -> Int
---------------------------- (d <- Int)
((flip twice) 1) doble :: Int

-- 5
. appDup = \f -> (\x -> f (x,x))
. appFork = \(f, g) -> (\x -> (f x, g x))
. appPar = \(f, g) -> (\(x, y) -> (f x, g y))
. appDist = \f -> (\(x, y) -> (f x, f y))
. subst = \f -> (\g -> (\x -> (f x) (g x)))

-- 6
-- a
compose :: (y -> z) -> (x -> y) -> x -> z
fst :: (a,b) -> a
snd :: (c,d) -> d
compose (fst snd) ::
-- (fst snd) no tiene tipo porque fst espera una tupla y snd es una funcion.
-- compose fst snd si tiene tipo.

-- b
uncurry :: (a -> b -> c) -> (a,b) -> c
curry :: ((d,e) -> f) -> d -> e -> f
snd :: (x,y) -> y
(uncurry curry snd) :: (x,y) -> y

-- c
apply :: (a -> b) -> a -> b
id :: c -> c
apply id :: c -> c
id apply :: (a -> b) -> a -> b
apply :: (a -> b) -> a -> b
(apply id) ((id apply) apply) ::
-- ((id apply) apply) no tiene tipo porque una funcion de primer orden toma y devuelve un valor, mientras que de orden superior toma y devuelve funciones.
-- No tiene variante posible.

-- d
compose :: (a -> b) -> (c -> a) -> c -> b
compose :: (x -> y) -> (z -> x) -> z -> y
doble :: Int -> Int
doble :: Int -> Int
compose (compose doble doble) :: (c -> Int) -> c -> Int

-- e
(compose compose) doble doble
compose :: (a -> b) -> (c -> a) -> c -> b
compose :: (x -> y) -> (z -> x) -> z -> y
doble :: Int -> Int
doble :: Int -> Int
-- (compose compose) not tiene tipo porque una funcion de primer orden toma y devuelve un valor, mientras que de orden superior toma y devuelve funciones.

-- 7
many :: Int -> (a -> a) -> a -> a
many 0 f = id f
many n f = f . (many (n-1) f)

-- 8
-- a
(Int -> Int) -> (Int -> Int)
-- Es una función que . dada una funcion que toma un entero y devuelve un entero . devuelve una funcion que toma un entero y devuelve un entero.
(Int -> Int) -> Int -> Int
-- Es una función que . dada una funcion que toma un entero y devuelve un entero . y un entero . devuelve un entero.

-- b
(a -> (b -> c)) -> (a -> b) -> c
-- Es una función que . dada una funcion que toma un a y devuelve una funcion que toma un b y devuelve un c . y una funcion que toma un a y devuelve un b . devuelve un c.
(a -> b -> c) -> (a -> b) -> c
-- Es una función que . dada una funcion que toma un a y un b y devuelve un c . y una funcion que toma un  a y devuelve un b. devuelve un c.

-- c
(a -> b, c -> d) -> ((a, c) -> (b, d))
-- Es una función que . dada una funcion que toma una tupla (funcion que toma un a y devuelve un b, funcion que toma un c y devuelve un d) . devuelve una funcion que toma una tupla (a,c) y devuelve una tupla (b,d).
(a -> b, c -> d) -> (a, c) -> (b, d)
-- Es una función que . dada una funcion que toma una tupla (funcion que toma un a y devuelve un b, funcion que toma un c y devuelve un d) . y una tupla (a,c) . devuelve una tupla (b,d).

-- d
((a, a) -> b) -> (a -> b)
-- Es una función que . dada una funcion que toma una tupla (a,a) y devuelve un b . devuelve una funcion que toma un a y devuelve un b.
((a, a) -> b) -> a -> b
-- Es una función que . dada una funcion que toma una tupla (a,a) y devuelve un b . y un a . devuelve un b.

-- e
(a -> (b -> c)) -> (b -> (a -> c))
-- Es una funcion que . dada una funcion que toma un a y devuelve una funcion que toma un b y devuelve un c . devuelve una funcion que toma un b y devuelve una funcion que toma un a y devuelve un c.
(a -> b -> c) -> b -> a -> c
-- Es una funcion que . dada una funcion que toma un a y un b y devuelve un c . y un b . y un a . devuelve un c. 

-- f
(a -> b) -> ((a, a) -> (b, b))
-- Es una funcion que . dada una funcion que toma un a y devuelve un b . devuelve una funcion que toma una tupa (a,a) y devuelve una tupla (b,b).
(a -> b) -> (a, a) -> (b, b)
-- Es una funcion que . dada una funcion que toma un a y devuelve un b . y una tupa (a,a) . devuelve una tupla (b,b).

-- g
(a -> b, a -> c) -> (a -> (b, c))
-- Es una funcion que . dada una tupla (funncion que toma un a y devuelve un b, funcion que toma un a y devuelve un c) . devuelve una funcion que toma un a y devuelve una tupla (b,c).
(a -> b, a -> c) -> a -> (b, c)
-- Es una funcion que . dada una tupla (funncion que toma un a y devuelve un b, funcion que toma un a y devuelve un c) . y un a . devuelve una tupla (b,c).

-- h
(a -> (b -> c)) -> ((a -> b) -> (a -> c))
-- Es una funcion que . dada una funcion que toma un a y devuelve una funcion que toma un b y devuelve un c . devuelve una funcion, que toma una funcion que toma a y devuevle b, y devuelve una funcion que toma un a y devuelve un c.
(a -> b -> c) -> (a -> b) -> (a -> c)
-- Es una funcion que . dada una funcion que toma un a y un b y devuelve un c . y una funcion que toma a y devuevle b . devuelve una funcion que toma un a y devuelve un c.

-- i
a -> (b -> a)
-- Es una funcion que . dado un a . devuelve una funcion que toma un b y devuelve un a.
a -> b -> a
-- Es una funcion que . dado un a . y un b . devuelve un a.

-- 9
cuadruple x = doble (doble x) = doble . doble
timesTwoPlusThree x = suma (doble x) 3 = flip suma 3 . doble
fourTimes f x = f (f (f (f x))) = twice . twice

-- 10
-- Las secciones de operadores en Haskell son una notación para la aplicación parcial de operadores infijos.
-- Permiten crear una función nueva simplemente encerrando un operador infijo entre paréntesis y proporcionándole solo uno de sus operandos.
-- (+ 1) Función que suma 1 a su argumento. (+ 1) 5.
-- (* 2) Función que multiplica su argumento por 2. (* 2) 3.

-- a1
-- a
suma :: Int -> (Int -> Int)
suma x = g where g y = x + y

suma :: Int -> (Int -> Int)
(suma x) y = x + y

suma :: Int -> Int -> Int
suma x y = x + y

-- b
apply :: (a -> b) -> (a -> b)
apply f = g where g x = f x

apply :: (a -> b) -> (a -> b)
(apply f) x = f x

apply :: (a -> b) -> a -> b
apply f x = f x

-- c
const :: a -> (b -> a)
const x = g where g y = x

const :: a -> (b -> a)
(const x) y = x 

const :: a -> b -> a
const x y = x 

-- d
compose :: (a -> b) -> ((c -> a) -> c -> b)
compose f = h where h g = k where k x = f (g x)

compose :: (a -> b) -> (c -> a) -> c -> b
((compose f) g) x = f (g x)

compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f (g x)

-- e
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = h where h x = k where k y = (f y) x

flip :: (a -> b -> c) -> b -> a -> c
((flip f) x) y = (f y) x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- f
subst :: (a -> b) -> ((c -> a) -> c -> b)
subst f = h where h g = k where k x = (f x) (g x)

subst :: (a -> b) -> (c -> a) -> c -> b
((subst f) g) x = (f x) (g x)

subst :: (a -> b) -> (c -> a) -> c -> b
subst f g x = f (g x)