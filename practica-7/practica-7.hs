-- 1
-- Regla Base:
    Prepizza esta en Pizza.

-- Regla Inductiva:
    Si p esta en Pizza
    e i esta en Ingrediente,
    Capa i p esta en 
    
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