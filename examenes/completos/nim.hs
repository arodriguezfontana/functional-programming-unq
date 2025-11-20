-- 1. Esquemas.
foldN :: b -> (Int -> b -> b) -> Nim -> b
foldN z hf Empty = z 
foldN z hf (Heap i n)= hf i (foldN z hf n) 

recN :: b -> (Int -> Nim -> b -> b) -> Nim -> b
recN z hf Empty = z 
recN z hf (Heap i n)= hf i n (recN z hf n) 

foldG :: b -> (Move -> b -> b -> b) -> GameTree -> b
foldG z f Nil = z
foldG z f (Node (m, hijo) hermano) = f m (foldG z f hijo) (foldG z f hermano)

recG :: b -> (Move -> GameTree -> GameTree -> b  -> b -> b) -> GameTree -> b
recG z f Nil = z
recG z f (Node (m, hijo) hermano) = f m hijo hermano (recG z f hijo) (recG z f hermano)

foldG' :: ((Move, b) -> b -> b) -> b -> GameTree -> b
foldG' _ z Nil = 
foldG' f z (Node m g) = f (fst m, foldG f z (snd m)) (foldG f z g)

recG' :: ((Move, b) -> GameTree -> GameTree -> b  -> b) -> b -> GameTree -> b
recG' _ z Nil = z
recG' f z (Node m g) = let (m', g') = m
                        in f (m', recG f z (snd m)) g' g (recG f z g)

-- 2. Esquemas.
-- a. Indica la cantidad de pilas en un juego de Nim dado.
heaps :: Nim -> Int
heaps = foldN 0 (\_ rec -> 1 + rec)

-- b .Indica la cantidad de fichas en un juego de Nim dado.
chips :: Nim -> Int
chips = foldN 0 (\n rec -> n + rec)

-- c. Computa la cantidad de fichas en el heap más grande.
maxHeap :: Nim -> Int
maxHeap = foldN 0 (\n rec -> max n rec)

-- d. Une dos juegos de Nim en uno solo que contiene las pilas de ambos en orden. Por ejemplo, el juego de la Figura 1b puede obtenerse como merge de los juegos de las Figuras 1a y 2.
alongside :: Nim -> Nim -> Nim -- dos parametros, b = (Nim -> Nim)
alongside n1 n2 = (foldN id (\n rec -> \base -> Heap n (rec base)) n1) n2

-- e. Retorna la altura de un GameTree, considerado como árbol general (por ejemplo, el árbol de la Figura 2 tiene altura de juego 3, porque el juego más largo que se puede jugar contiene 3 jugadas).
gameHeight :: GameTree -> Int
gameHeight = foldG
    (\(m, hHijo) hHermano -> max (1 + hHijo) hHermano) 
    0

-- f. Retorna la lista de ramas en un GameTree, considerado como árbol general. Una rama es el desarrollo de un juego completo de Nim hasta que haya un ganador.
branches :: GameTree -> [[Move]]
branches = recG
    (\(m, rHijo) gHijo gHermano rHermano -> 
        (case gHijo of 
            Nil -> [[m]]              
            _   -> map (m:) rHijo     
        ) ++ rHermano) 
    []

-- 3. Esquemas.
-- a. Lleva a cabo una jugada si es posible.
turn :: Nim -> Move -> Maybe Nim
turn = recN 
        (const Nothing)
        (\n mnim nim (p, c) -> if p == 0
                                then Just (Heap (max (n-c) 0) nim)
                                else Just (Heap n (maybeToNim (mnim (p-1,c)))))

maybeToNim :: Maybe Nim -> Nim
maybeToNim Nothing = Empty
maybeToNim (Just nim) = nim

-- b. Retorna la lista de jugadas válidas del jugador actual.
moves :: Nim -> [Move]
moves = foldN (\n ms -> moveHasta n ++ map (\(p, c) -> (p+1, c)) ms) []

moveHasta :: Int -> [Move]
moveHasta 0 = []
moveHasta n = moveHasta (n-1) ++ [(0, n)]

-- c. Construye el árbol de juego con todas las jugadas válidas.
solve :: Nim -> GameTree
solve Empty = Nil
solve nim = genGT (moves nim) nim

genGT :: [Move] -> Nim -> GameTree
genGT = foldr
    (\m fms nim -> Node (m, solve (maybeToNim (turn nim m))) (fms nim))
    (const Nil)

-- d. Indica si el jugador 1 tiene una estrategia ganadora, es decir, si puede ganar de alguna forma, sin importar las jugadas que haga el contrincante.
winning :: Nim -> Bool
winning nim = hasWinningMove (solve nim)

hasWinningMove :: GameTree -> Bool
hasWinningMove Nil = False
hasWinningMove (Node (_, hijo) hermano) = cannotWin hijo || hasWinningMove hermano

cannotWin :: GameTree -> Bool.
cannotWin Nil = True
cannotWin (Node (_, hijo) hermano) = hasWinningMove hijo && cannotWin hermano