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
    cuadruple 2
->                  (cuadruple, x <- 2)
    2 * 4
->                  (Aritmetica)
    8

    cuadruple (cuadruple 2) -- Interno
->                              (cuadruple, x <- 2)
    cuadruple (2 * 4)
->                              (Aritmetica)    
    cuadruple 8             
->                              (cuadruple, x <- 8)
    8 * 4
->                              (Aritmetica)
    32

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

    sumarDos 2
->              (sumarDos, x <- 2)
    2 + 2
->              (Aritmetica)
    4

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


-- E7
    ((twice twice) doble) 3

-- E8
triple = (/x -> x * 3)
succ = (/x -> x + 1)
sumarDos = (/x -> x + 2)
twice = (/f (/x -> f (f x)))
twice twice = (/f (/x -> f (f (f (f x)))))

-- E9

-- EA1

    (\f -> f2 + f4) id
->                          (Beta)
    id 2 + id 4
->                          (id, x <- 2)
    2 + id 4
->                          (id, x <- 4)
    2 + 4
->                          (Aritmetica)
    6

-- EA2
    (\f -> f2 + f4) doble
->                          (Beta)
    doble 2 + doble 4
->                          (doble, x <- 2)
    (2 + 2) + doble 4
->                          (doble, x <- 4)
    (2 + 2) + (4 + 4)
->                          (Aritmetica)
    12

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