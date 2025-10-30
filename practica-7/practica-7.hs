-- S1
--1
Regla Base:
    Prepizza esta en Pizza.

Regla Inductiva:
    Si p esta en Pizza
    e i esta en Ingrediente,
    Capa i p esta en Pizza.
    
-- 2
f :: Pizza -> a
f Prepizza = ...
f (Capa i p) = ... i ... (f p) ...

-- 3
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa i p) = case i of
    Aceituna n -> n + cantidadDeAceitunas p
    _ -> cantidadDeAceitunas p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = case i of
    Aceituna n -> Capa (Aceituna (n*2)) (duplicarAceitunas p)
    _ -> Capa i (duplicarAceitunas p)

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = case i of
    Queso -> sinLactosa p
    _ -> Capa i (sinLactosa p)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa i p) = case i of
    Queso -> False
    _ -> aptaIntolerantesLactosa p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = agregarCapa i (conDescripcionMejorada p)

agregarCapa :: Ingrediente -> Pizza -> Pizza
agregarCapa (Aceituna n) (Capa (Aceituna m) p) = Capa (Aceituna (n+m)) p
agregarCapa i p = Capa i p

-- 4
-- a
Izq:
    cantidadDeAceitunas Prepizza

Der:
    cantidadDeAceitunas
        (conDescripcionMejorada Prepizza)
=                                                           (conDescripcionMejorada)
    cantidadDeAceitunas Prepizza

-- b
Izq:
    cantidadDeAceitunas (Capa Queso Prepizza)

Der:
    cantidadDeAceitunas
        (conDescripcionMejorada (Capa Queso Prepizza))
=                                                                               (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa Queso
            (conDescripcionMejorada Prepizza))
=                                                                               (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa Queso Prepizza)
=                                                                               (agregarCapa)
    cantidadDeAceitunas (Capa Queso Prepizza)

-- c
Izq:
    cantidadDeAceitunas (Capa (Aceitunas 8)
        (Capa Queso Prepizza))
    
Der:
    cantidadDeAceitunas
        (conDescripcionMejorada (Capa (Aceitunas 8)
            (Capa Queso Prepizza)))
=                                                           (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa (Aceitunas 8)
            (conDescripcionMejorada (Capa Queso Prepizza)))      
=                                                           (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa (Aceitunas 8)
            (agregarCapa Queso
                (conDescripcionMejorada Prepizza)))
=                                                           (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa (Aceitunas 8)
            (agregarCapa Queso Prepizza))
=                                                           (agregarCapa)
    cantidadDeAceitunas
        (agregarCapa (Aceitunas 8) (Capa Queso Prepizza)
=                                                           (agregarCapa)
    cantidadDeAceitunas (Capa (Aceituna 8)
        (Capa Queso Prepizza))

-- d
Izq:
    cantidadDeAceitunas (Capa (Aceitunas 9)
        (Capa (Aceitunas 8)
            (Capa Queso Prepizza)))
=                                           (cantidadDeAceitunas)
    9 + cantidadDeAceitunas
        (Capa (Aceitunas 8)
            (Capa Queso Prepizza))
=                                           (cantidadDeAceitunas)
    9 + 8 + cantidadDeAceitunas
                (Capa Queso Prepizza))
=                                           (cantidadDeAceitunas)
    9 + 8 + cantidadDeAceitunas Prepizza
=                                           (cantidadDeAceitunas)
    9 + 8 + 0
=                                           (Aritm.)
    17

Der:
    cantidadDeAceitunas
        (conDescripcionMejorada
            (Capa (Aceitunas 9)
                (Capa (Aceitunas 8)
                    (Capa Queso Prepizza))))
=                                               (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa (Aceitunas 9)
            (conDescripcionMejorada
                (Capa (Aceitunas 8)
                    (Capa Queso Prepizza)))
=                                               (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa (Aceituna 9)
            (agregarCapa (Aceituna 8)
                (conDescripcionMejorada
                    (Capa Queso Prepizza))))
=                                               (conDescripcionMejorada)
    cantidadDeAceitunas
        (agregarCapa (Aceituna 9)
            (agregarCapa (Aceituna 8)
                (agregarCapa Queso Prepizza)))
=                                                (agregarCapa)
    cantidadDeAceitunas
        (agregarCapa (Aceituna 9)
            (agregarCapa (Aceituna 8)
                (Capa Queso Prepizza)))
=                                               (agregarCapa)
    cantidadDeAceitunas
        (agregarCapa (Aceituna 9)
            (Capa (Aceituna 8)
                (Capa Queso Prepizza)))
=                                               (agregarCapa)
    cantidadDeAceitunas
        (Capa (Aceituna (9 + 8))
            (Capa Queso Prepizza))
=                                               (Aritm.)
    cantidadDeAceitunas
        (Capa (Aceituna 17)
            (Capa Queso Prepizza))
=                                               (cantidadDeAceitunas)
    17 + cantidadDeAceitunas
        (Capa Queso Prepizza)
=                                               (cantidadDeAceitunas)
    17 + cantidadDeAceitunas Prepizza
=                                               (cantAceitunas)
    17 + 0
=                                               (Aritm.)
    17

-- S2
-- 1
-- 2
-- 3
-- 4
-- 5

-- S3
-- 1
Regla Base:
    Si x :: a,
    entonces Habitacion x está en Dungeon a.

Regla Inductiva 1:
    Si m :: Maybe a,
    d :: Dungeon a,
    entonces Pasaje m d está en Dungeon a.

Regla Inductiva 2:
    Si m :: Maybe a,
    d1 :: Dungeon a,
    d2 :: Dungeon a,
    entonces Bifurcacion m d1 d2 está en Dungeon a

-- 2
f (Habitacion x) = ...
f (Pasaje m d) = ... m ... f d
f (Bifurcacion m d1 d2) = ... m ... f d1 ... f d2

-- 3
-- a
cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion _) = 0
cantidadDeBifurcaciones (Pasaje _ d) = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion _ d d2) = 1 + cantidadDeBifurcaciones d + cantidadDeBifurcaciones d2

-- b
cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion x) = 1
cantidadDePuntosInteresantes (Pasaje m d) = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d d2) = 1 + cantidadDePuntosInteresantes d + cantidadDePuntosInteresantes d2

-- c
cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion x) = 0
cantidadDePuntosVacios (Pasaje m d) = unoSiEsVacio m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d d2) = unoSiEsVacio m + cantidadDePuntosVacios d + cantidadDePuntosVacios d2

unoSiEsVacio :: Maybe a -> Int
unoSiEsVacio Nothing = 1
unoSiEsVacio _ = 0

-- d
cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon x (Habitacion y) = unoSiIguales x y
cantidadDePuntosCon x (Pasaje m d) = unoSiIguales x (fromJust m) + cantidadDePuntosCon x d
cantidadDePuntosCon x (Bifurcacion m d d2) = unoSiIguales x (fromJust m) + cantidadDePuntosCon x d + cantidadDePuntosCon x d2

unoSiIguales :: Eq a => a -> a -> Bool
unoSiIguales x y = if x == y then 1 else 0

-- e
esLineal :: Dungeon -> Bool
esLineal (Habitacion _) = True
esLineal (Pasaje _ d) = esLineal d
esLineal (Bifurcacion _ _ _) = False

-- f
llenoDe :: a -> Dungeon -> Bool
llenoDe x (Habitacion y) = x == y
llenoDe x (Pasaje m d) = case m of
    Just y  -> x == y && llenoDe x d
    Nothing -> False
llenoDe x (Bifurcacion m d1 d2) = case m of
    Just y  -> x == y && llenoDe x d1 && llenoDe x d2
    Nothing -> False

-- 4
-- a
Izq:
    cantidadDePuntosVacios (Habitacion x)
-- = (cantidadDePuntosVacios)
    0

Der:
    0

-- b
Izq:
    cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio Nothing + cantidadDePuntosVacios (Habitacion Joyas)
-- = (unoSiEsVacio)
    1 + cantidadDePuntosVacios (Habitacion Joyas)
-- = (cantidadDePuntosVacios)
    1 + 0
-- = (Aritm.)
    1

Der:
    1

-- c
Izq:
    cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio (Just y) + cantidadDePuntosVacios (Habitacion x)
-- = (unoSiEsVacio)
    0 + cantidadDePuntosVacios (Habitacion x)
-- = (cantidadDePuntosVacios)
    0 + 0
-- = (Aritm.)
    0

Der:
    0

-- d
Izq:
    cantidadDePuntosVacios (Bifurcacion Nothing 
        (Pasaje Nothing (Habitacion Joyas))
        (Pasaje (Just Oro) (Habitacion Cofre)))
-- = (cantidadDePuntosVacios)
     unoSiEsVacio Nothing
     + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
     + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
-- = (unoSiEsVacio)
    1 + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
-- = (cantidadDePuntosVacios)
    1 + (unoSiEsVacio Nothing + cantidadDePuntosVacios (Habitacion Joyas))
    + (unoSiEsVacio (Just Oro) + cantidadDePuntosVacios (Habitacion Cofre))
-- = (unoSiEsVacio)
    1 + (1 + cantidadDePuntosVacios (Habitacion Joyas))
    + (0 + cantidadDePuntosVacios (Habitacion Cofre))
-- = (cantidadDePuntosVacios)
    1 + (1 + 0) + (0 + 0)
-- = (Asoc.)
    1 + 1 + 0 + 0 + 0
-- = (Aritm.)
    2

Der:
    2

-- e
Izq:
    cantidadDePuntosVacios (Bifurcacion Nothing
        (Pasaje Nothing (Habitacion z))
        (Pasaje (Just y) (Habitacion x)))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio Nothing
    + cantidadDePuntosVacios (Pasaje Nothing (Habitacion z))
    + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio Nothing
    + (unoSiEsVacio Nothing + cantidadDePuntosVacios (Habitacion z))
    + (unoSiEsVacio (Just y) + cantidadDePuntosVacios (Habitacion x))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio Nothing
    + (unoSiEsVacio Nothing + 0)
    + (unoSiEsVacio (Just y) + 0)
-- = (unoSiEsVacio)
    1 + (1 + 0) + (0 + 0)
-- = (Asoc. y Aritm.)
    2

Der:
    2

-- f
Izq:
    cantidadDePuntosVacios
        (Bifurcacion (Just Cofre)
            (Bifurcacion Nothing
                (Pasaje Nothing (Habitacion Joyas))
                (Pasaje (Just Oro) (Habitacion Cofre)))
            (Bifurcacion Nothing
                (Pasaje (Just Oro) (Habitacion Oro)
                (Pasaje Nothing (Habitacion Joyas))))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio (Just Cofre)
    + cantidadDePuntosVacios (Bifurcacion Nothing
                (Pasaje Nothing (Habitacion Joyas))
                (Pasaje (Just Oro) (Habitacion Cofre))
    + cantidadDePuntosVacios (Bifurcacion Nothing
                (Pasaje (Just Oro) (Habitacion Oro)
                (Pasaje Nothing (Habitacion Joyas)))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio (Just Cofre)
    + unoSiEsVacio Nothing
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
    + unoSiEsVacio Nothing
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Oro)
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
-- = (cantidadDePuntosVacios)
    unoSiEsVacio (Just Cofre)
    + unoSiEsVacio Nothing
        + unoSiEsVacio Nothing + cantidadDePuntosVacios (Habitacion Joyas)
        + unoSiEsVacio (Just Oro) + cantidadDePuntosVacios (Habitacion Cofre)
    + unoSiEsVacio Nothing
        + unoSiEsVacio (Just Oro) + cantidadDePuntosVacios (Habitacion Oro)
        + unoSiEsVacio Nothing + cantidadDePuntosVacios (Habitacion Joyas)
-- = (cantidadDePuntosVacios)
    unoSiEsVacio (Just Cofre)
    + (unoSiEsVacio Nothing
        + (unoSiEsVacio Nothing + 0)
        + (unoSiEsVacio (Just Oro) + 0))
    + (unoSiEsVacio Nothing
        + (unoSiEsVacio (Just Oro) + 0)
        + (unoSiEsVacio Nothing + 0))
-- = (unoSiEsVacio)
    0 + 1 + 1 + 0 + 0 + 0 + 1 + 0 + 0 + 1 + 0
-- = (Aritm.)
    4

Der:
    4

-- 5
-- a
Izq:
    cantidadDePuntosCon (Criatura Troll)
        (Habitacion (Objeto Oro)) 
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (Objeto Oro)
-- = (unoSiIguales)
    if (Criatura Troll) == (Objeto Oro) then 1 else 0
-- = (Eval. de ==)
    if False then 1 else 0
-- = (Eval. de condicional)
    0

Der:
    0

-- b
Izq:
    cantidadDePuntosCon (Criatura Troll)
        (Pasaje (Just (Criatura Troll)) (Habitacion (Objeto Oro))))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
-- = (fromJust)
    unoSiIguales (Criatura Troll) (Criatura Troll)
    + cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Objeto Oro)
-- = (unoSiIguales)
    if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Objeto Oro) then 1 else 0
-- = (Eval. de ==)
    if True then 1 else 0
    + if False then 1 else 0
-- = (Eval. de condicional)
    1 + 0
-- = (Aritm.)
    1

Der:
    1

-- c
Izq:
    cantidadDePuntosCon (Criatura Troll)
        (Pasaje (Just (Criatura Troll))
            (Habitacion (Criatura Troll)))) 
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll) (Habitacion (Criatura Troll))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (Criatura Troll)
-- = (fromJust)
    unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
-- = (unoSiIguales)
    if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
-- = (Eval. de ==)
    if True then 1 else 0
    + if True then 1 else 0
-- = (Eval. de condicional)
    1 + 1
-- = (Aritm.)
    2

Der:
    2

-- d
Izq:
    cantidadDePuntosCon (Criatura Troll)
        (Bifurcacion (Just (Criatura Troll))
            (Pasaje (Just (Criatura Troll))
                (Habitacion (Objeto Oro)))
            (Pasaje (Just (Criatura Troll))
                (Habitacion (Criatura Troll))))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll)
        (Pasaje (Just (Criatura Troll))
                (Habitacion (Objeto Oro)))
    + cantidadDePuntosCon (Criatura Troll)
        (Pasaje (Just (Criatura Troll))
                (Habitacion (Criatura Troll)))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Criatura Troll))
    + unoSiIguales (Criatura Troll) (fromJust (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
    + unoSiIguales (Criatura Troll) (fromJust (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll) (Habitacion (Criatura Troll))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Criatura Troll))
    + unoSiIguales (Criatura Troll) (fromJust (Criatura Troll))
    + unoSiIguales (Criatura Troll) (Objeto Oro)
    + unoSiIguales (Criatura Troll) (fromJust (Criatura Troll))
    + unoSiIguales (Criatura Troll) (Criatura Troll)
-- = (fromJust)
    unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Objeto Oro)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
-- = (unoSiIguales)
    if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Objeto Oro) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
-- = (Eval. de ==)
    if True then 1 else 0
    + if True then 1 else 0
    + if False then 1 else 0
    + if True then 1 else 0
    + if True then 1 else 0
-- = (Eval. de condicional)
    1 + 1 + 0 + 1 + 1
-- = (Aritm.)
    4

Der:
    4

-- e
Izq:
    cantidadDePuntosCon (Criatura Troll)
        (Pasaje (Just (Criatura Troll))
            (Bifurcacion (Just (Criatura Troll))
                (Pasaje (Just (Criatura Troll))
                    (Habitacion (Objeto Oro)))
                (Pasaje (Just (Criatura Troll))
                    (Habitacion (Criatura Troll)))))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll)
            (Bifurcacion (Just (Criatura Troll))
                (Pasaje (Just (Criatura Troll))
                    (Habitacion (Objeto Oro)))
                (Pasaje (Just (Criatura Troll))
                    (Habitacion (Criatura Troll)))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll)
        (Pasaje (Just (Criatura Troll))
                    (Habitacion (Objeto Oro))
    + cantidadDePuntosCon (Criatura Troll) 
        (Pasaje (Just (Criatura Troll))
                    (Habitacion (Criatura Troll))
-- = (cantidadDePuntosCon)
    unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro)
    + unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + cantidadDePuntosCon (Criatura Troll) (Habitacion (Criatura Troll)
-- = (cantidadDePuntosCon)
     unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (Objeto Oro)
    + unoSiIguales (Criatura Troll) (fromJust (Just (Criatura Troll))
    + unoSiIguales (Criatura Troll) (Criatura Troll)
-- = (fromJust)
    unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Objeto Oro)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
    + unoSiIguales (Criatura Troll) (Criatura Troll)
-- = (unoSiIguales)
    if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Objeto Oro) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
    + if (Criatura Troll) == (Criatura Troll) then 1 else 0
-- = (Eval. de ==)
    if True then 1 else 0
    + if True then 1 else 0
    + if True then 1 else 0
    + if False then 1 else 0
    + if True then 1 else 0
    + if True then 1 else 0
-- = (Eval. de condicional)
    1 + 1 + 1 + 0 + 1 + 1
-- = (Aritm.)
    5

Der:
    5