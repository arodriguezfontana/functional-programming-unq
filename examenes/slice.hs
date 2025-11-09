data SliceExp a = Base [a] 
                    | Take Int (SliceExp a)
                    | Drop Int (SliceExp a) deriving (Show)

-- 1. Recursion explicita.

materialize :: SliceExp a -> [a]
-- Devuelve la lista final, luego de evaluar todas las operaciones.

lenS :: SliceExp a -> Int
-- Devuelve la longitud final de un SliceExp. 

normalize :: SliceExp a -> SliceExp a
-- Simplifica expresiones de manera tal que no hay dos Take ni dos Drop seguidos, y solo numeros positivos. 

takeS :: Int -> SliceExp a -> SliceExp a
-- Aplica take a una expresión SliceExp ya normalizada, sin evaluarla completamente. La expresion que devuelve no inicia con el contrsuctor Take. 

-- 2. Demostrar: lenS . normalize = lenS.


-- 3. Esquema primitivo y recursivo de SliceExp a. 


-- 4. Ejecicio 1 sin recursion explicita. 