-- 1
(/x -> x + x) 2
(/x -> x + 3) 1
(twice doble) 1
doble (doble 1)
cuadruple 1
(\f -> f (f 1)) doble
(\f -> (\g -> f (g 1))) doble doble
(\g -> doble (g 1)) doble

-- 2
    doble (doble 2)
->                      (doble, x <- doble 2)
    doble 2 + doble 2
->                      (doble, x <- 2)
    (2 + 2) + doble 2
->                      (doble, x <- 2)
    (2 + 2) + (2 + 2)   
->                      (Aritmetica)
    8

--  3
-- a
    cuadruple 2
->                  (cuadruple, x <- 2)
    2 * 4
->                  (Aritmetica)
    8

-- b
    cuadruple (cuadruple 2)
->                              (cuadruple, x <- 2)
    cuadruple (2 * 4)
->                              (Aritmetica)    
    cuadruple 8             
->                              (cuadruple, x <- 8)
    8 * 4
->                              (Aritmetica)
    32

-- c
    cuadruple (cuadruple 2)
->                              (cuadruple, x <- cuadruple 2)
    (cuadruple 2) * 4
->                              (cuadruple, x <- 2) 
    (2 * 4) * 4
->                              (Aritmetica)
    32              

-- 4
triple :: Int -> Int
triple x = x * 3

succ :: Int -> Int
succ x = x + 1

sumarDos :: Int -> Int
sumarDos x = x + 2

-- 5
-- a
    sumarDos n
->              (sumarDos, x <- n)
    n + 2

-- b
    twice succ n
->                  (twice, f <- succ)
    g n             (g x = succ (succ x))
->                  (g, x <- n)
    succ (succ n)
->                  (succ, x <- n)
    succ n + 1
->                  (succ, x <- n)
    n + 1 + 1
->                  (Aritmetica)
    n + 2

-- 6
doble = \x -> 2 * x
twice doble = cuadruple
twice id = id

-- 7
    ((twice twice) doble) 3
->                                  (twice, f <- twice)
    (g doble) 3                     (g x = twice (twice x) )
->                                  (g, x <- doble)
    (twice (twice doble)) 3 
->                                  (twice, f' <- twice doble)
    g 3                             (g' x' = twice doble (twice doble x'))
->                                  (g', x' <- 3)
    twice doble (twice doble 3)
->                                  (twice, f'' <- doble)
    twice doble (g 3)               (g'' x'' = doble (doble x''))
->                                  (g, x'' <- 3)
    twice doble (doble (doble 3))
->                                  (doble, x <- 3)
    twice doble (doble (3 + 3))
->                                  (Aritmetica)
    twice doble (doble 6)
->                                  (doble, x' <- 6)
    twice doble (6 + 6)
->                                  (Aritmetica)
    twice doble 12
->                                  (twice, f''' <- doble)
    g 12                            (g''' x''' = doble (doble x'''))
->                                  (g''', x''' <- 12)
    doble (doble 12)
->                                  (doble, x'' <- 12)
    doble (12 + 12)
->                                  (Aritmetica)
    doble 24            
->                                  (doble, x''' <- 24)
    24 + 24
->                                  (Aritmetica)
    48

-- 8
triple = /x -> x * 3
succ = /x -> x + 1
sumarDos = /x -> x + 2
twice = /f (/x -> f (f x))
twice twice = /f (/x -> f (f (f (f x))))

-- 9
f x = x
f (x,y) = x
f (x,y) = x
f (x,y) = y

-- a1
-- a
    (\f -> f2 + f4) id
->                          (Beta)
    id 2 + id 4
->                          (id, x <- 2)
    2 + id 4
->                          (id, x <- 4)
    2 + 4
->                          (Aritmetica)
    6

-- b
    (\f -> f2 + f4) doble
->                          (Beta)
    doble 2 + doble 4
->                          (doble, x <- 2)
    (2 + 2) + doble 4
->                          (doble, x <- 4)
    (2 + 2) + (4 + 4)
->                          (Aritmetica)
    12

-- c
    (\f -> f2 + f4) (suma 17)
->                              (Beta)
    (suma 17) 2 + (suma 17) 4
->                              (suma, x <- 17)
    g 2 + (suma 17) 4           (g y = 17 + y)
->                              (g, y <- 2)
    (17 + 2) + (suma 17) 4
->                              (suma, x <- 17)
    (17 + 2) + g 4              (g y = 17 + y)
->                              (g, y <- 4)
    (17 + 2) + (17 + 4)
->                              (Aritmetica)
    40

-- a2
-- a
    (suma 2) 3
->              (suma, x <-2)
    g 3         (g y = 2 + y)
->              (g, y <- 3)
    2 + 3
->              (Aritmetica)
    5

-- b
    ((subst const) suma) 17
->                              (subst, f <- const)
    (h suma) 17                 (h g = k)
->                              (h, g <- suma)
    k 17                        (k x, (const x) (suma x))
->                              (k, x <- 17)
    (const 17) (suma 17)
->                              (const, x <- 17)
    g (suma 17)                 (g y = 17)
->                              (g, y <- suma 17)
    17

-- c
    ((subst const) twice) doble
->                                  (subst, f <- const)
    (h twice) doble                 (h g = k)
->                                  (h, g <- twice)
    k doble                         (k x = (const x) (twice x))
->                                  (k, x <- doble)
    (const doble) (twice doble)
->                                  (const, x <- doble)
    g (twice doble)                 (g y = doble)
->                                  (g, y <- twice doble)
    doble

-- a3
(subst const) suma = id
\f -> (subst const) f = const id

-- a4
id = \x -> x
const = \x -> (\y -> x)
flip = \f -> (\x -> (\y -> (f y) x)))

-- a5
yTambien :: Bool -> Bool -> Bool
yTambien b = if b
                then id
                else const False