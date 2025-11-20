-- 1. Esquemas.
foldNim :: b -> (Int -> b -> b) -> Nim -> b 
foldNim z h Empty        = z 
foldNim z h (Heap n nim) = h n (foldNim z h nim)

recNim :: b -> (Int -> b -> Nim -> b) -> Nim -> b 
recNim z h Empty        = z 
recNim z h (Heap n nim) = h n (recNim z h nim) nim 

foldGame :: b -> ((Move, b) -> b -> b) -> GameTree -> b
foldGame z f Nil                   = z
foldGame z f (Node (mov, gm1) gm2) = f (mov, (foldGame z f gm1)) (foldGame z f gm2)

recGame :: b -> ((Move, b) -> GameTree -> b -> GameTree -> b) -> GameTree -> b
recGame z f Nil                   = z
recGame z f (Node (mov, gm1) gm2) = f (mov, (recGame z f gm1)) gm1 (recGame z f gm2) gm2

-- 2
-- indica la cantidad de pilas en un juego de  Nim  dado.
heaps :: Nim -> Int
heaps Empty        = 0 
heaps (Heap n nim) = 1 + heaps nim 

heaps' :: Nim -> Int
heaps' = foldNim 0 (\_ m -> 1 + m)

-- Indica la cantidad de fichas  en un juego de  Nim  dado.
chips :: Nim -> Int 
chips Empty        = 0 
chips (Heap n nim) = n + chips nim 

chips' :: Nim -> Int 
chips' = foldNim 0 (+)

-- Computa la cantidad de fichas en el heap más grande.
maxHeap :: Nim -> Int  
maxHeap Empty        = 0
maxHeap (Heap n nim) = max n (maxHeap nim)

maxHeap' :: Nim -> Int 
maxHeap' = foldNim 0 max 

-- Une  dos  juegos  de  Nim  en  uno  solo  que  contiene  las  pilas  de  ambos  en  orden.  Por  ejemplo,  el  juego  de  la  Figura  1b puede  obtenerse  como  merge de los juegos de las Figuras 1a y 2. 
alongside :: Nim -> Nim -> Nim
alongside Empty         nim2 = nim2 
alongside (Heap n nim1) nim2 = Heap n (alongside nim1 nim2)

alongside' :: Nim -> Nim -> Nim
alongside' = foldNim id (\n f -> \nim2 -> Heap n (f nim2))

-- Retorna  la  altura  de  un  GameTree  ,  considerado  como  árbol  general  (por  ejemplo,  el  árbol  de  la  Figura  2  tiene  altura de  juego  3,  porque  el  juego  más largo que se puede jugar contiene 3 jugadas). 
gameHeight :: GameTree -> Int
gameHeight Nil                 = 0 
gameHeight (Node (_, gm1) gm2) = 1 + max (gameHeight gm1) (gameHeight gm2 - 1)

gameHeight' :: GameTree -> Int
gameHeight' = foldGame 0 (\(mov, r1) r2 -> 1 + max r1 (r2 - 1))

-- Retorna  la  lista  de  ramas  en  un  GameTree, considerado  como  árbol  general.  Una  rama  es  el  desarrollo  de  un  juego  completo  de  Nim  hasta que haya un ganador. 
branches :: GameTree -> [[Move]]
branches Nil                   = []
branches (Node (mov, gm1) gm2) = case branches gm1 of
                                    [] -> [[mov]]
                                    bs -> map (mov :) bs ++ branches gm2 

branches' :: GameTree -> [[Move]]
branches' = foldGame [] (\(mov, movss1) movss2 -> case movss1 of 
                                                    [] -> [[mov]] 
                                                    bs -> map (mov :) movss1 ++ movss2)

-- 3

-- Lleva a cabo  una jugada si es posible.
turn :: Nim -> Move -> Maybe Nim   
turn Empty        mov   = Nothing
turn (Heap n nim) (i,k) = if i == 0 
                            then if n > k
                                then Just (Heap (n-k) nim)
                                else Nothing 
                            else case turn nim (i-1,k) of 
                                Nothing   -> Nothing            -- Jugada invalida
                                Just nim' -> Just (Heap n nim') -- Heap luego de la jugada

-- Retorna la lista de jugadas válidas del jugador actual. 
moves :: Nim -> [Move]
moves nim = movesAux nim 0

movesAux :: Nim -> Int -> [Move]
movesAux Empty        _ = []
movesAux (Heap m nim) i = replicateT i m ++ movesAux nim (i + 1)

replicateT :: Int -> Int -> [(Int, Int)]
replicateT i 1 = [(i,1)]
replicateT i n = (i,n) : replicateT i (n-1)

movesAux' :: Nim -> Int -> [Move]
movesAux' = foldNim (const []) (\m f -> \i -> replicateT i m ++ f (i+1))

replicateT' :: Int -> Int -> [(Int, Int)]
replicateT' = flip (foldN1 (const []) (\k acc -> \i -> (i, k) : acc i))

foldN1 :: a -> (Int -> a -> a) -> Int -> a
foldN1 z f 1 = f 1 z 
foldN1 z f n = f n (foldN1 z f (n-1))

-- Construye el árbol de  juego con todas las jugadas válidas. 
solve :: Nim -> GameTree
solve nim = genGT nim (moves nim)

genGT :: Nim -> [Move] -> GameTree
genGT _ []     = Nil
genGT nim (m:ms) = case turn nim m of
                    Nothing     -> genGT nim ms 
                    Just nim'   -> Node (m, solve nim') (genGT nim ms)

-- d. Indica si el jugador 1 tiene una estrategia ganadora, es decir, si puede ganar de alguna forma, sin importar las jugadas que haga el contrincante.
winning :: Nim -> Bool
winning nim = hasWinningMove (solve nim)

hasWinningMove :: GameTree -> Bool
hasWinningMove Nil = False
hasWinningMove (Node (_, hijo) hermano) = cannotWin hijo || hasWinningMove hermano

cannotWin :: GameTree -> Bool.
cannotWin Nil = True
cannotWin (Node (_, hijo) hermano) = hasWinningMove hijo && cannotWin hermano