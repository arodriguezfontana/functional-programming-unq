-- 1
-- a
udiv (x,y) = div x y
-- Parcial, falla si el segundo componente de la tupla es 0.

-- b
udivE (x,0) = error "No puedo dividir por 0"
udivE (x,y) = div x y
-- Parcial, maneja el caso pero lanzando un error, no un valor de tipo valido.

-- c
udivH = uncurry div
-- Parcial, falla si el segundo valor que recibe div es 0.

-- d
succ x = x + 1
-- Total, definida para todos los valores de su dominio.

-- e
succH = suma 1
-- Total, definida para todos los valores de su dominio.

-- f
porLaMitad = flip div 2
-- Total, definida para todos los valores de su dominio.

-- g
conDieresis 'u' = 'ü'
-- Parcial, definida solo para el caracter 'u', sino devuelve bottom.

-- h
conDieresisB 'u' = 'ü'
conDieresisB c = conDieresisB c
-- Parcial, definida solo para el caracter 'u', sino genera una recursión infinita, por ende, bottom.

-- i
conTildePM 'a' = 'á'
conTildePM 'e' = 'é'
conTildePM 'i' = 'í'
conTildePM 'o' = 'ó'
conTildePM 'u' = 'ú'
-- Parcial, definida solo para las vocales minusculas, sino devuelve bottom.

-- j
conTildeE c = if esVocal c
    then conTildePM c
    else error "El valor recibido no es vocal"
-- Parcial, maneja el caso pero lanzando un error, no un valor de tipo valido.

-- k
conTilde c = if esVocal c && esMinuscula c
    then conTildePM c
    else c
-- Total, definida para todos los valores de su dominio.

-- 2
-- a = c, d = e

-- 3
twice doble -- 1 (twice doble)
twice doble 2 -- 4 (twice doble, (\x -> doble (doble x)) 2, doble 2, doble 4)
twice -- 0

-- 4
twice doble -- 1 (twice doble)
twice doble 2 -- 4 (twice doble, g 2, doble 2, doble 4)
twice -- 0

-- 5
twice doble -- 1 (twice doble)
twice doble 2 -- 3 (twice doble 2, doble 2, doble 4)
twice -- 0

-- 6
-- En todos los casos, los tipos son demasiado generales (polimórficos)
-- como para construir valores concretos distintos de ⊥.
-- Por eso, no hay dos expresiones diferentes y definidas que los habiten.

-- a1
-- a2
-- a3
-- a4