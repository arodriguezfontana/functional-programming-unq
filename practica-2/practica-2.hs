-- E1
-- a
first :: (a,b) -> a
(x,y) :: (a,b)
------------------
first (x,y) :: a

x :: a
y :: b
------------------
(x,y) :: (a,b)

-- b
apply :: (a -> b) -> (a -> b)
f :: a -> b
-----------
apply f :: a -> b

g :: a -> b
x :: a
-----------
g x :: b

f :: a -> b
x :: a
---------------
f x :: b

-- c
twice :: (a -> a) -> (a -> a)
f :: a -> a
---------------------
twice f :: a -> a

g :: a -> a
x :: a
-----------
g x :: a

f :: a -> a
f x :: a
--------------
f (f x) :: a 

f :: a -> a
x :: a
-----------
f x :: a

-- d
doble :: Int -> Int
x :: Int
--------------
doble x :: Int

x :: Int
x :: Int
------------
x + x :: Int

-- e
swap :: (a,b) -> (b,a)
(x,y) :: (a,b)
------------------------
swap (x,y) :: (b,a)

x :: a
y :: b
-------------------
(x,y) :: (a,b)

-- f
unflip :: ((b,a) -> c) -> ((a,b) -> c)
f :: (b,a) -> c
-----------
unflip f :: (a,b) -> c

g :: (a,b) -> c
p :: (a,b)
-------------
g p :: c

f :: (b,a) -> c
swap p :: (b,a)
---------------
f (swap p) :: c

swap :: (a,b) -> (b,a)
p :: (a,b)
------------
swap p :: (b,a)

-- E2
-- a
apply :: (a -> b) -> (a -> b)
first :: (c,d) -> c
------------------------- (a <- (c,d), b <- c)
apply first :: (c,d) -> c

-- b
first :: -> (a,b) -> a
(swap, uflip) :: ((c,d) -> (d,c), ((d,c) -> e) -> ((c,d) -> e))
------------------------------------- (a <- (c,d) -> (d,c), b <- ((d,c) -> e) -> ((c,d) -> e))
first (swap, uflip) :: (c,d) -> (d,c)

-- c
twice :: (a -> a) -> (a -> a)
doble :: Int -> Int
------------------------- (a <- Int)
twice doble :: Int -> Int

-- d
twice :: (a -> a) -> (a -> a)
twice :: (b -> b) -> (b -> b)
------------------------------------ (a <- (b -> b))
twice twice :: (b -> b) -> (b -> b)

-- e
twice :: (a -> a) -> (a -> a) 
uflip :: ((c,b) -> d) -> ((b,c) -> d)
--------------------
twice uflip :: 

-- f
twice :: (a -> a) -> (a -> a) 
swap :: (b,c) -> (c,b)
--------------------- (
twice swap :: 

-- g
uflip :: ((b,a) -> c) -> ((a,b) -> c)
swap :: (a,b) -> (b,a)
uflip swap ::

-- h
twice twice ::
swap ::
(twice twice) swap :: 