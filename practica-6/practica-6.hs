-- 1
-- a
doble = \x -> 2 * x
por ppio. de ext.:
    para todo y. doble y = (\x -> 2 * x) y

Sea n::Int

Izq:
    doble n 
=           (doble)
    n + n

Der:
    (\x -> 2 * x) n
=                   (Beta., x <- n)
    2 * n
=                   (Aritm.)
    n + n

-- b
compose doble doble = cuadruple
por ppio. de ext.:
    para todo x. compose doble doble x = cuadruple x

Sea n::Int

Izq:
    compose doble doble n
=                           (compose)
    doble (doble n)
=                           (doble)
    doble (n+n)
=                           (doble)
    (n+n) + (n+n)
=                           (Asoc.)
    n + n + n + n
=                           (Aritm.)
    4 * n

Der:
    cuadruple n
=               (cuadruple)
    4 * n

-- 2
-- a
para todo x,y. x && y = not ((not x) || (not y))

Sea x'::Bool
Sea y'::Bool

Der:
    x' && y'-- Verdadero si x' = True && y = True

Izq:
    not ((not x') || (not y'))
=                               (de morgan)
    (not (not x')) && (not (not y'))
=                               (doble negacion)
    x' && y'

-- b
para todo x,y. not (x || y) = not x && not y

Sea x'::Bool
Sea y'::Bool

Der:
    not (x || y)
=                   (de morgan)
    x && y -- Verdadero si x' = True && y = True
    
Izq:
    not x && not y
=                   (not)
    x && y

-- A1
-- a
para todo f. subst const f = id
por ppio. de ext.:
    para todo f, x. subst const f x = id x

Sea f'::(a->b)
Sea x'::a

Izq:
    subst const f' x' 
=                       (subst)
    (const x') (f' x')
=                       (const)
    x'

Der:
    id x'
=           (id)
    x'

-- b
curry fst = const

por ppio. de ext.:
  para todo x. curry fst x = const x
  para todo x,y. curry fst x y = const x y

Sea x'::a
Sea y'::b

Izq:
    curry fst x' y'
=                   (curry)
    fst (x', y')
=                   (fst)
    x'

Der: 
    const x' y'
=               (const)
    x'