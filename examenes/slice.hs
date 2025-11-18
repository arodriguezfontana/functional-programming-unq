-- 1. Recursion explicita.
-- a. Devuelve la lista final, luego de evaluar todas las operaciones.
materialize :: SliceExp a -> [a]
materialize (Base xs) = xs
materialize (Take n s) = take n (materialize s)
materialize (Drop n s) = drop n (materialize s)

-- b. Devuelve la longitud final de un SliceExp. 
lenS :: SliceExp a -> Int
lenS (Base xs) = length xs
lenS (Take n s) = min n (lenS s)
lenS (Drop n s) = max 0 (lenS s - n)

-- c. Simplifica expresiones de manera tal que no hay dos Take ni dos Drop seguidos, y solo numeros positivos. 
normalize :: SliceExp a -> SliceExp a
normalize (Base xs) = Base xs
normalize (Take n s) = if n <= 0
    then normalize s
    else normTake n (normalize s)
normalize (Drop n s) = if n <= 0
    then normalize s
    else normDrop n (normalize s)

normTake :: Int -> SliceExp a -> SliceExp a 
normTake n (Take m s) = Take (n+m) s
normTake n s = Take n s

normDrop :: Int -> SliceExp a -> SliceExp a
normDrop n (Drop m s) = Drop (n+m) s
normDrop n s = Drop n s

takeS :: Int -> SliceExp a -> SliceExp a
-- Aplica take a una expresión SliceExp ya normalizada, sin evaluarla completamente. La expresion que devuelve no inicia con el contrsuctor Take. 

-- 2. Demostrar: lenS . normalize = lenS.
por ppio. de ext.:
para todo se:
    ¿(lenS . normalize) se = lenS se?
-- (.)
    ¿lenS (normalize se) = lenS se?

sea
se'::SliceExp a
quiero ver que:
    ¿lenS (normalize se') = lenS se'?

por ppio. de ind. sobre la estructura se',
es eq. a demostrar:
    cb, se'=(Base xs)
        ti: ¿lenS (normalize (Base xs)) = lenS (Base xs)?
    ci1, se'=(Take n s)
        hi: ¡lenS (normalize s) = lenS s!
        ti: ¿lenS (normalize (Take n s)) = lenS (Take n s)?
    ci2, se'=(Drop n s)
        hi: ¡lenS (normalize s) = lenS s!
        ti: ¿lenS (normalize (Drop n s)) = lenS (Drop n s)?

cbi:
lenS (normalize (Base xs))
-- normalize.1
lensS (Base xs)
-- lensS.1
length xs

cbd:
lenS (Base xs)
-- lensS.1
length xs

cb demostrado.

ci1i:
lenS (normalize (Take n s))

ci1d:
lenS (Take n s)

ci1 demostrado.

ci2i:
lenS (normalize (Drop n s))

ci2d:
lenS (Drop n s)

ci2 demostrado.

-- 3. Esquema primitivo y recursivo de SliceExp a. 
foldSE :: ([a] -> b) -> (Int -> b -> b) -> (Int -> b -> b) -> SliceExp a -> b
foldSE bf tf df (Base xs) = bf xs
foldSE bf tf df (Take n s) = tf n (foldSE bf tf df s)
foldSE bf tf df (Drop n s) = df n (foldSE bf tf df s)

recSE :: ([a] -> b) -> (Int -> SliceExp a -> b -> b) -> (Int -> SliceExp a -> b -> b) -> SliceExp a -> b
recSE bf tf df (Base xs) = bf xs
recSE bf tf df (Take n s) = tf n s (recSE bf tf df s)
recSE bf tf df (Drop n s) = df n s (recSE bf tf df s)

-- 4. Utilizando esquemas.
materialize' :: SliceExp a -> [a]
materialize' = foldSE 
    (\xs -> xs)
    (\n xs -> take n xs)
    (\n xs -> drop n xs)

lenS' :: SliceExp a -> Int
lenS' = foldSE
    (\xs -> length xs)
    (\n m -> min n m)
    (\n m -> max 0 (m-n))

normalize' :: SliceExp a -> SliceExp a
normalize' = foldSE
    (\xs -> Base xs)
    (\n s -> if n <= 0
        then s
        else normTake n s)
    (\n s -> if n <= 0
        then s
        else normDrop n s))

-- d
takeS' :: Int -> SliceExp a -> SliceExp a
