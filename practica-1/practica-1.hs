-- E1
(/x -> x + x) 2
(/x -> x + 3) 1
(twice doble) 1
doble (doble 1)
cuadruple 1
(\f -> f (f 1)) doble
(\f -> (\g -> f (g 1))) doble doble
(\g -> doble (g 1)) doble

-- E2
    doble (doble 2)
->                      (doble, x <- doble 2)
    doble 2 + doble 2
->                      (doble, x <- 2)
    (2 + 2) + doble 2
->                      (doble, x <- 2)
    (2 + 2) + (2 + 2)   
->                      (Aritmetica)
    8

--  E3
-- a
    cuadruple 2
->                  (cuadruple, x <- 2)
    2 * 4
->                  (Aritmetica)
    8

---b
    cuadruple (cuadruple 2) -- Interno
->                              (cuadruple, x <- 2)
    cuadruple (2 * 4)
->                              (Aritmetica)    
    cuadruple 8             
->                              (cuadruple, x <- 8)
    8 * 4
->                              (Aritmetica)
    32

-- c    
    cuadruple (cuadruple 2) -- Externo
->                              (cuadruple, x <- cuadruple 2)
    (cuadruple 2) * 4
->                              (cuadruple, x <- 2) 
    (2 * 4) * 4
->                              (Aritmetica)
    32              

-- E4
triple :: Int -> Int
triple x = x * 3

succ :: Int -> Int
succ x = x + 1

sumarDos :: Int -> Int
sumarDos x = x + 2

-- E5
twice succ 2 = sumarDos 2

-- a
    sumarDos 2
->              (sumarDos, x <- 2)
    2 + 2
->              (Aritmetica)
    4

-- b
    twice succ 2
->                  (twice, f <- succ)
    g 2             (g x = succ (succ x))
->                  (g, x <- 2)
    succ (succ 2)
->                  (succ, x <- 2)
    succ 2 + 1
->                  (succ, x <- 2)
    2 + 1 + 1
->                  (Aritmetica)
    4

-- E6
doble = (\x -> 2 * x)
twice doble = cuadruple
twice id = id

-- E7
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

-- E8
triple = (/x -> x * 3)
succ = (/x -> x + 1)
sumarDos = (/x -> x + 2)
twice = (/f (/x -> f (f x)))
twice twice = (/f (/x -> f (f (f (f x)))))

-- E9

-- EA1
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

-- EA2
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

-- c
    ((subst const) twice) doble

-- EA3
(subst const) suma = id
\f -> (subst const) f = id

-- EA4
id = (\x -> x)
const = (\x (\y -> x))
flip = (\f -> (\x -> (\y -> (f y) x))))