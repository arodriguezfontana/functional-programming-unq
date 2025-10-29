-- 1
-- a
doble = \x -> 2 * x
por principio de extensionaldiad:
    ¿para todo y. doble y = (\x -> 2 * x) y?

sea n un Int culquiera, quiero ver que:
    doble n = (\x -> 2 * x) n 

Izq:
    doble n 
=           (doble)
    n + n

Der:
    (\x -> 2 * x) n
=                   (Beta)
    2 * n
=                   (Aritm.)
    n + n

-- b
compose doble doble = cuadruple
por principio de extensionaldiad:
    ¿para todo x. compose doble doble x = cuadruple x?

sea n un Int cualquiera, quiero ver que:
    compose doble doble n = cuadruple n

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
para todo x, para todo y. x && y = not ((not x) || (not y))

sean b y c dos Bool cualquiera, quiero ver que:
    b && c = not ((not b) || (not c))
por analisis de casos. 

-- b = True y c = True
Der:
    True && True
=                   (&&)
    True

Izq:
    not ((not True) || (not True))
=                                   (not)
    not (False || False)
=                                   (||)
    not False
=                                   (not)
    True

-- b = True y c = False
Der:
    True && False
=                   (&&)
    False

Izq:
    not ((not True) || (not False))
=                                   (not)
    not (False || True)
=                                   (||)
    not True
=                                   (not)
    False

-- b = False y c = True
Der:
    False && True
=                   (&&)
    False

Izq:
    not ((not False) || (not True))
=                                   (not)
    not (True || False)
=                                   (||)
    not True
=                                   (not)
    False

-- b = False y c = False
Der:
    False && False
=                   (&&)
    False

Izq:
    not ((not False) || (not False))
=                                   (not)
    not (True || True)
=                                   (||)
    not True
=                                   (not)
    False

-- b
para todo x, para todo y. not (x || y) = not x && not y

sean b y c dos Bool cualquiera, quiero ver que:
    b && c = (b || c) = not b && not c
por analisis de casos.

-- b = False y c = True 
Der:
    not (False || True)
=                       (||)
    not True
=                       (not)
    False
    
Izq:
    not False && not True
=                           (not)
    True && False
=                           (&&)
    False

-- b = True y c = False 
Der:
    not (True || False)
=                       (||)
    not True
=                       (not)
    False
    
Izq:
    not True && not False
=                           (not)
    False && True
=                           (&&)
    False

-- b = False y c = False 
Der:
    not (False || False)
=                       (||)
    not False
=                       (not)
    True
    
Izq:
    not False && not False
=                           (not)
    True && True
=                           (&&)
    True

-- 3
-- a
por principio de extensionaldiad:
    ¿para todo x, curry suma' x = suma x?
    ¿para todo x, para todo y, curry suma' x y = suma x y'?

sean n y m dos Int cualquiera, quiero ver que:
    curry suma' n m = suma n m

Izq:
    curry suma' n m
=                       (curry)
    suma' (n,m)
=                       (suma')
    n + m

Der:
    suma n m
=               (suma)
    n + m

-- b
por principio de extensionaldiad:
    ¿para todo x, uncurry suma x = suma' x?
    ¿para todo x, para todo y, uncurry suma (x,y) = suma' (x,y)?

sean n y m dos Int cualquiera, quiero ver que:
    uncurry suma (n,m) = suma' (n,m)

Izq:
    uncurry suma (n,m) = suma' (n,m)
=                                       (uncurry)
    suma n m
=                                       (suma)
    n + m

Der:
    suma' (n,m)
=               (suma')
    n + m

-- 4
-- a
por ppio. de ext.:
  para todo x. curry fst x = const x
  para todo x, para todo y. curry fst x y = const x y

sean x' e y' elementos cualquiera, quiero ver que:
    curry fst x' y' = const x' y'

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

-- b
por ppio. de ext.:
  para todo x. uncurry (flip const) x = snd x
  para todo x, para todo y. uncurry (flip const) (x,y) = snd (x,y)

sean a y b elementos cualquiera, quiero ver que:
    uncurry (flip const) (a,b) = snd (a,b)

Izq:
    uncurry (flip const) (a,b)
=                               (uncurry)
    flip const a b
=                               (flip)
    const b a
=                               (const)
    b 

Der:
    snd (a,b)
=               (snd)
    b

-- 5
-- a
para todo f. curry (uncurry f) = f
    para todo f. para todo x. curry (uncurry f) x = f x
    para todo f. para todo x. para todo y. curry (uncurry f) x y = f x y

sea g una funcion cualquiera y a y b elementos cualquiera, quiero ver que:
    curry (uncurry g) a b = g a b

Izq:
    curry (uncurry g) a b
=                           (curry)
    uncurry g (a,b)
=                           (uncurry)
    g a b
    
Der:
    g a b

-- b
para todo f'. uncurry (curry f') = f'
    para todo f'. para todo x. uncurry (curry f') x = f' x
    para todo f'. para todo x. para todo y. uncurry (curry f') (x,y) = f' (x,y)

sea g una funcion cualquiera y a y b elementos cualquiera, quiero ver que:
    uncurry (curry g) (a,b) = g (a,b)

Izq:
    uncurry (curry g) (a,b)
=                               (uncurry)
    curry g a b 
=                               (curry)
    g (a,b)

Der:
    g (a,b)

-- 6
-- 7

-- a1
-- a
para todo f. subst const f = id
por principio de extensionaldiad:
    para todo f, para todo x. subst const f x = id x

sean f' una funcion cualquiera y x' un elemento cualquiera, quiero ver que:
    subst const f' x' = id x'

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
  para todo x, para todo y. curry fst x y = const x y

sean x' e y' elementos cualquiera, quiero ver que:
    curry fst x' y' = const x' y'

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