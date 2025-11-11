data Accion a = Paso a 
    | SaltoArriba a
    | SaltoAdelante a
    | Girar a
type Tiempo = Int
type Duracion = Int
data Animacion a = Espera Duracion
    | Mov Duracion (Accion a)
    | Sec (Animacion a) (Animacion a)
    | Par (Animacion a) (Animacion a)

-- 1: Debe realizar UN único recorrido sobrecada lista.
combinarSinDuplicados :: [Int] -> [Int] -> [Int]

-- 2: Recursión explícita.
-- a
duracion :: Animacion a -> Int,

-- b
alargar :: Int -> Animacion a -> Animacion a

-- c
simular :: Animacion a -> [Frame a]

-- d
tiemposDeEspera :: Animacion a -> [Tiempo]

-- 3: Demostrar que para todo k >= 0. duracion . (alargar k) = (k*) . duracion

-- 4: Recursion estructural y primitiva para Animacion.

-- 5: Ejercicio 2 utilizando esquemas.

-- 6: Utilizando esquemas.
-- a
ciclar :: Animacion a -> Simulador a

-- b
combinar :: [Animacion a] -> [Animacion a] -> Animacion a

-- c
mezclar :: [Simulador a] -> Duracion -> [Frame a]