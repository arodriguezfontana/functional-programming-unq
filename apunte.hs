-- f 
take ::: Int -> [a] -> [a]

f :: [a] -> b
f [] = ...
f (x:xs) = ... x ... f xs ...

f = foldr (\x r -> ... x ... r) (... xs)
-- Parametrisacion de los puntitos
-- 1ra funcion de 2 argumentos
-- El while no termina, no lo puedo escribir con fold

-- foldr toma una función combinadora, un valor base y la lista.
-- Función combinadora (f): Debe tomar el elemento actual (n) y el resultado recursivo (r), y combinarlos.
-- Valor base (z): Es el valor para la lista vacía.44

-- x simepre el primer elemento
-- r el resultado de la recursion n la cola
-- cuando escribo una duncion por recurcion ponfo la duncion SIN PARAMETROS

-- IZQUIERDA, NO ES CON LO QUE HAGO RECURCION Y NO CAMBIA
-- r tiene un nombre adecuado al tipo del resultado
-- si cambia lo paso para der

-- si tiene dos parametros es una funcion que toma un parametro y devuelve ua fnucion que toma otro


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

g :: [a] -> b
g [] = ...
g (x:xs) = ... x ... g xs

g = foldr (\x r -> ... x ... r) (...)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs)

g :: [a] -> b
g [] = ...
g (x:xs) = x ... xs ... g xs ...

-- parte inductiva sin haber hecho recursion

maximun :: [a] -> a
maximun [] = error ""
maximun (x:xs)= if null xs
    then xs
    else max x (maximun xs)

maximun = recr (error "") (\x xs maximo -> if null xs
                                                then x
                                                else max x maximun)

id :: a -> a
map :: (a -> b) -> [a] -> [b]
twice :: (a -> a) -> a -> a
const :: a -> b -> a
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
foldr :: (a -> b -> b) -> b -> [a] -> b
a -> b

filter :: (a -> Bool) -> [a] -> [a]
flip :: (a -> b -> c) -> b -> a -> c
Int -> Int -> Int

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
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([], [])
unzip ((x,y):xys) = let (xs, ys) = (unzip xys) 
                        in (x:xs, y:ys)

max :: Ord a => a -> a -> a
max x y = if x >= y then x else y

null :: [a] -> Bool
null [] = True
null (_:_) = False

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
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)

evalExpA :: ExpA -> Int
evalExpA (Cte n) = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2