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