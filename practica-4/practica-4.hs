-- E1
-- Total: Te doy algo que no es bottom y me devolves algo que no es bottom
-- Parcial: Te doy algo que no es bottom y me devolves bottom
-- No estricta: Te doy bottom y me devolves algo que no es bottom
-- Estricta: Te doy bottom y me devolves bottom

-- a
udiv (x,y) = div x y
-- Parcial
-- Falla si el segundo componente de la tupla es 0.

-- b
udivE (x,0) = error "No puedo dividir por 0"
udivE (x,y) = div x y
-- Parcial
-- Maneja la falla pero lanzando un error, no un valor de tipo valido.

-- c
udivH = uncurry div
-- Parcial
-- Falla si el segundo valor que recibe div es 0.

-- d
succ x = x + 1
-- Total
-- Definida para todos los valores de su dominio.

-- e
succH = suma 1
-- Total
-- Definida para todos los valores de su dominio.

-- e
porLaMitad = flip div 2
-- Total
-- La division por 2 esta definida para todos los valores de su dominio.

-- f
conDieresis 'u' = 'ü'
-- Parcial
-- Definida solo para el caracter 'u', si le paso otro devuelve bottom.

-- g
conDieresisB 'u' = 'ü'
conDieresisB c = conDieresisB c
-- Parcial
-- Solo devuelve algo valido para el caracter 'u', si le paso otro genera una recursión infinita, por ende, bottom.

-- h
conTildePM 'a' = 'á'
conTildePM 'e' = 'é'
conTildePM 'i' = 'í'
conTildePM 'o' = 'ó'
conTildePM 'u' = 'ú'
-- Parcial
-- Falla si le paso un caracter que no es vocal minuscula porque no hay definición, por ende, bottom.

-- j
conTildeE c = if esVocal c
    then conTildePM c
    else error "El valor recibido no es vocal"
-- Parcial
-- Maneja el caso pero lanzando un error.

-- k
conTilde c = if esVocal c && esMinuscula c
    then conTildePM c
    else c
-- Total
-- Definida para todos los valores

-- E2
-- a = c, d = e, i = j

-- E3