sizeTT :: ThreeT a -> Int
sizeTT (Leaf x) = 1
sizeTT (Branch x t1 t2 t3) = 1 + sizeTT t1 + sizeTT t2 + sizeTT t3

sumTT :: ThreeT Int -> ThreeT Int
sumTT (Leaf n) = Leaf n
sumTT (Branch n t1 t2 t3) = let t1' = sumTT t1
                                t2' = sumTT t2
                                t3' = sumTT t3
    in Branch (n + nodoDe t1' + nodoDe t2' + nodoDe t3') t1' t2' t3'

nodoDe :: ThreeT Int -> Int
nodoDe (Leaf n) = n
nodoDe (Branch n t1 t2 t3) = n

leavesTT :: ThreeT a -> [a]
leavesTT (Leaf x) = [x]
leavesTT (Branch x t1 t2 t3) = leavesTT t1 ++ leavesTT t2 ++ leavesTT t3

mapTT :: (a -> b) -> ThreeT a -> ThreeT b
mapTT f (Leaf x) = Leaf (f x)
mapTT f (Branch x t1 t2 t3) = Branch (f x) (mapTT f t1) (mapTT f t2) (mapTT f t3)

maxTT :: Ord a => ThreeT a -> a
maxTT (Leaf x) = x
maxTT (Branch x t1 t2 t3) = max x (max (maxTT t1) (max (maxTT t2) (maxTT t3))) 

findTT :: Eq a => (a -> Bool) -> ThreeT (a,b) -> Maybe b
findTT p (Leaf (x,y)) = if p x then Just y else Nothing
findTT p (Branch (x,y) t1 t2 t3) = if p x 
                                    then Just y 
                                    else elPrimerJust (findTT p t1) (findTT p t2) (findTT p t3)

elPrimerJust :: Maybe b -> Maybe b -> Maybe b -> Maybe b
elPrimerJust (Just y) _ _ = Just y
elPrimerJust _ (Just y) _ = Just y
elPrimerJust _ _ (Just y) = Just y
elPrimerJust _ _ _ = Nothing

levelNTT :: Int -> ThreeT a -> [a]
levelNTT n (Leaf x) = if n == 0
                        then [x]
                        else []
levelNTT n (Branch x t1 t2 t3) = if n > 0
                                  then levelNTT (n-1) t1 ++ levelNTT (n-1) t2 ++ levelNTT (n-1) t3
                                  else []

listPerLevelTT :: ThreeT a -> [[a]]
listPerLevelTT (Leaf x) = [[x]]
listPerLevelTT (Branch x t1 t2 t3) = [x] : combinar3 (listPerLevelTT t1) 
                                                     (listPerLevelTT t2) 
                                                     (listPerLevelTT t3)

combinar3 :: [[a]] -> [[a]] -> [[a]] -> [[a]]
combinar3 l1 l2 l3 = juntar l1 (juntar l2 l3)

combinar2 :: [[a]] -> [[a]] -> [[a]]
combinar2 [] ys = ys              
combinar2 xs [] = xs  
combinar2 (x:xs) (y:ys) = (x ++ y) : combinar2 xs ys

-- 2. Esquemas.
foldTT :: (a -> b) -> (a -> b -> b -> b -> b) -> ThreeT a -> b
foldTT lf bf (Leaf x) = lf x
foldTT lf bf (Branch x t1 t2 t3) = bf x (foldTT lf bf t1) (foldTT lf bf t2) (foldTT lf bf t3)

recTT :: (a -> b) -> (a -> ThreeT a -> ThreeT a -> ThreeT a -> b -> b -> b -> b) -> ThreeT a -> b
recTT lf bf (Leaf x) = lf x
recTT lf bf (Branch x t1 t2 t3) = bf x t1 t2 t3 (foldTT lf bf t1) (foldTT lf bf t2) (foldTT lf bf t3)

-- 3. Con esquemas.
sizeTT' :: ThreeT a -> Int
sizeTT' = foldTT 
    const 1
    (\x n1 n2 n3 -> 1 + n1 + n2 + n3)

sumTT' :: ThreeT Int -> ThreeT Int
sumTT' = foldTT
          Leaf
          (\n t1 t2 t3 -> Branch (n + nodoDe t1 + nodoDe t2 + nodoDe t3) t1 t2 t3)

leavesTT' :: ThreeT a -> [a]
leavesTT' = foldTT
              (\x -> [x])
              (\x t1 t2 t3 -> t1 ++ t2 ++ t3)

mapTT' :: (a -> b) -> ThreeT a -> ThreeT b
mapTT' f = foldTT
            (\x -> Leaf (f x))
            (\x t1 t2 t3 -> Branch (f x) t1 t2 t3)

maxTT' :: Ord a => ThreeT a -> a
maxTT' = foldTT
          (\x t1 t2 t3 -> max x (max t1 (max t2 t3)))
          id

findTT' :: Eq a => (a -> Bool) -> ThreeT (a, b) -> Maybe b
findTT' p = foldTT
            (\(x,y) -> if p x then Just y else Nothing)
            (\(x,y) t1 t2 t3 -> if p x 
                                    then Just y 
                                    else elPrimerJust t1 t2 t3)

levelNTT' :: Int -> ThreeT a -> [a]
levelNTT' = flip (foldTT
              (\x -> \n -> if n == 0
                        then [x]
                        else []))
              (\x t1 t2 t3 -> \n -> if n > 0
                                  then t1 (n-1) ++ t2 (n-1) ++ t3 (n-1)
                                  else [])

listPerLevelTT' :: ThreeT a -> [[a]]
listPerLevelTT' = foldTT 
    (\x -> [[x]])                                  
    (\x rss1 rss2 rss3 -> [x] : combinar3 rss1 rss2 rss3)