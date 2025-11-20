-- 1. Recursion explicita.
-- a. Devuelve la lista final, luego de evaluar todas las operaciones.
materialize :: SliceExp a -> [a]
materialize (Base xs) = xs
materialize (Take n s) = take n (materialize s)
materialize (Drop n s) = drop n (materialize s)

-- b. Devuelve la longitud final de un SliceExp. 
lenS :: SliceExp a -> Int
lenS (Base xs) = length xs
lenS (Take n s) = max 0 (min n (lenS s))
lenS (Drop n s) = max 0 (lenS s - n)

-- c. Simplifica expresiones de manera tal que no hay dos Take ni dos Drop seguidos, y solo numeros positivos. 
normalize :: SliceExp a -> SliceExp a
normalize (Base xs) = Base xs
normalize (Take n s) = normTake (max 0 n) (normalize s)
normalize (Drop n s) = normDrop (max 0 n) (normalize s)

normTake :: Int -> SliceExp a -> SliceExp a
normTake m (Take n s) = Take (min n m) s
normTake m s = Take m s

normDrop :: Int -> SliceExp a -> SliceExp a
normDrop m (Drop n s) = Drop (n+m) s
normDrop m s = Drop m s

-- d. Aplica take a una expresión SliceExp ya normalizada, sin evaluarla completamente. La expresion que devuelve no inicia con el contrsuctor Take. 
takeS :: Int -> SliceExp a -> SliceExp a
takeS n (Base xs) = Base (take n xs)
takeS n (Take m s) = takeS (min n m) s
takeS n (Drop m s) = Drop m (takeS (n+m) s)

-- 2. Demostrar: lenS . normalize = lenS.
¿lenS (normalize s) = lenS s?

cb, s=Base xs: 
    ¿lenS (normalize (Base xs)) = lenS (Base xs)?
ci1, s=Take n s': 
    HI: ¡lenS (normalize s') = lenS s'!
    TI: ¿lenS (normalize (Take n s')) = lenS (Take n s')?
ci2, s=Drop n s':
    HI: ¡lenS (normalize s') = lenS s'!
    TI: ¿lenS (normalize (Drop n s')) = lenS (Drop n s')?

cb1i:
lenS (normalize (Base xs))
-- normalize.1
lenS (Base xs)

cb1d:
lenS (Base xs)

ci1i:
lenS (normalize (Take n s'))
-- normalize.2
lenS (normTake (max 0 n) (normalize s'))
-- lema.1: lenS (normTake n s) donde n>=0 = lenS (Take n s)
lenS (Take (max 0 n) (normalize s'))
-- lensS.2
max 0 (min (max 0 n) (lenS (normalize s')))
-- hi
max 0 (min (max 0 n) (lenS s'))
-- aritm. max 0 (min (max 0 n) L) == max 0 (min n L)
max 0 (min n (lenS s'))

ci1d:
lenS (Take n s')
-- lenS.2
max 0 (min n (lenS s'))

ci1 demostrado.

ci2i:
lenS (normalize (Drop n s'))
-- normalize.3
lenS (normDrop (max 0 n) (normalize s'))
-- Lema 2: lenS (normDrop n s)) donde n>=0 == lenS (Drop n s)
lenS (Drop (max 0 n) (normalize s'))
-- lenS.3
max 0 (lenS (normalize s') - (max 0 n))
-- hi
max 0 (lenS s' - (max 0 n))

ci2d: lenS (Drop n s')
-- lenS.3
max 0 (lenS s' - n)

ci1 demostrado.

Lema 1: lenS (normTake m s) = lenS (Take m s) (Asumiendo m >= 0)

Caso 1: s = Take k sub
c1i: lenS (normTake m (Take k sub))
-- normTake.1
lenS (Take (min k m) sub)
-- lenS.2
max 0 (min (min k m) (lenS sub))
-- Aritmética (Asociatividad de min)
max 0 (min m (min k (lenS sub)))

c1d:
lenS (Take m (Take k sub))
-- lenS.2
max 0 (min m (lenS (Take k sub)))
-- lenS.2
max 0 (min m (max 0 (min k (lenS sub))))
-- Aritmética (Como m >= 0, el max 0 interno es redundante frente al externo)
max 0 (min m (min k (lenS sub)))

Caso 2: s /= Take
c2i:
lenS (normTake m s)
-- normTake.2
lenS (Take m s)

c2d:
lenS (Take m s)

Lema 1 demostrado.

Lema 2: lenS (normDrop m s) = lenS (Drop m s) (Asumiendo m >= 0)

Caso 1: s = Drop k sub
c1i: lenS (normDrop m (Drop k sub))
-- normDrop.1
lenS (Drop (k + m) sub)
-- lenS.3
max 0 (lenS sub - (k + m))

c1d:
lenS (Drop m (Drop k sub))
-- lenS.3
max 0 (lenS (Drop k sub) - m)
-- lenS.3
max 0 ((max 0 (lenS sub - k)) - m)
-- Aritmética (Como m >= 0, restar m a un max 0 es igual a restar acumulado)
max 0 (lenS sub - k - m)
-- Aritmética
max 0 (lenS sub - (k + m))

Caso 2: s /= Drop
c2i:
lenS (normDrop m s)
-- normDrop.2
lenS (Drop m s)

c2d:
lenS (Drop m s)

Lema 2 demostrado.

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
materialize' = foldS id take drop

lenS' :: SliceExp a -> Int
lenS' = foldS
  length
  (\i rec  -> max 0 (min i rec))
  (\i rec  -> max 0 (rec - i))

normalize' :: SliceExp a -> SliceExp a
normalize' = foldS Base normTake normDrop

-- d
takeS' :: Int -> SliceExp a -> SliceExp a -- SliceExp a -> (Int  -> SliceExp a)
takeS' = flip (foldS
    (\xs m -> Base (take m xs))
    (\n res m -> res (min n m))
    (\n res m -> Drop n (res (m+n))))