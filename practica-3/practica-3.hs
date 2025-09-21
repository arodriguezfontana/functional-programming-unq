-- E1


-- E2
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

-- E3
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

-- E4
-- a
apply :: (a -> b) -> (a -> b)
apply :: (a -> b) -> (a -> b)
----------------------------------- (a <- (a -> b), b <- (a -> b))
apply apply :: (a -> b) -> (a -> b)

apply apply :: (a -> b) -> (a -> b)
apply :: (a -> b) -> (a -> b)
------------------------------------------- (a <- (a -> b), b <- (a -> b))
(apply apply) apply :: (a -> b) -> (a -> b)

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
twice :: (a -> a) -> (a -> a)
----------------------------------- (a <- (a -> a))
twice twice :: (a -> a) -> (a -> a)

twice twice :: (a -> a) -> (a -> a)
twice :: (a -> a) -> (a -> a)
------------------------------------------- (a <- (a -> a))
(twice twice) twice :: (a -> a) -> (a -> a)

(twice twice) twice :: (a -> a) -> (a -> a)
swap :: (a,b) -> (b,a)
-------------------------------------------- (b <- a, a <- (a,a))
((twice twice) twice) swap :: (a,a) -> (a,a)

-- d
flip :: (a -> (b -> c)) -> (b -> (a -> c))
twice :: (a -> a) -> (a -> a)
------------------ ((b -> c) <- a, a <- (a -> a))
flip twice ::

flip twice :: Int ->
1 :: Int
(flip twice) 1 ::

(flip twice) 1 :: (Int -> Int) ->
doble :: Int -> Int
((flip twice) 1) doble :: 