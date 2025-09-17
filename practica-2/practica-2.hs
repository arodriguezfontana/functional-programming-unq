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