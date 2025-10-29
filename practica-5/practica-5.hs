-- 1
Vasito :: Gusto -> Helado
Chocolate :: Gusto
Cucurucho :: Gusto -> Gusto -> Helado
Sambayon :: Gusto
Pote :: Gusto -> Gusto -> Gusto -> Helado
chocoHelate :: (Gusto -> a) -> a
chocoHelate Vasito :: Helado
chocoHelate Cucurucho :: Gusto -> Helado
chocoHelate (Cucurucho Sambayon) :: Helado
chocoHelate (chocoHelate Cucurucho) :: Helado
chocoHelate (Vasito DulceDeLeche) -- No posee tipo
chocoHelate Pote :: Gusto -> Gusto -> Helado
chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

--2
dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool :: DigBin -> Boo
dbAsInt O = False
dbAsInt I = True

dbOfBool :: Bool -> DigBin
dbOfBool False = O
dbOfBool True = I

negDB :: DigBin -> DigBin
negDB O = I
negDB I = O

-- 3
ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

ddAsInt :: Int -> DigDec
ddAsInt 0 = D0
ddAsInt 1 = D1
ddAsInt 2 = D2
ddAsInt 3 = D3
ddAsInt 4 = D4
ddAsInt 5 = D5
ddAsInt 6 = D6
ddAsInt 7 = D7
ddAsInt 8 = D8
ddAsInt 9 = D9
ddAsInt _ = error "El numero debe estar entre 0 y 9"

nextDD :: DigDec -> DigDec
nextDD D0 = D1
nextDD D1 = D2
nextDD D2 = D3
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0

prevDD :: DigDec -> DigDec
prevDD D0 = D9
prevDD D1 = D0
prevDD D2 = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8

-- 4
asMm :: Medida -> Medida
asMm (Mm m) = Mm m
asMm (Cm c) = Mm (c * 10.0)
asMm (Inch i) = Mm (i * 25.4)
asMm (Foot f) = Mm (f * 304.8)

asCm :: Medida -> Medida
asCm (Mm m) = Cm (m * 0.1)
asCm (Cm c) = Cm c
asCm (Inch i) = Cm (i * 2.54)
asCm (Foot f) = Cm (f * 30.48)

asInch :: Medida -> Medida
asInch (Mm m) = Inch (m * 0.039)
asInch (Cm c) = Inch (c * 0.394)
asInch (Inch i) = Inch i
asInch (Foot f) = Inch (f * 12.0)

asFoot :: Medida -> Medida
asFoot (Mm m) = Foot (m * 0.003)
asFoot (Cm c) = Foot (c * 0.033)
asFoot (Inch i) = Foot (pulgadas * 0.083)
asFoot (Foot f) = Foot f

-- 5
uncurry Rect :: (Float, Float) -> Shape
construyeShNormal (flip Rect 5.0) :: Shape
compose (uncurry Rect) swap :: (Float, Float) -> Shape
uncurry Cucurucho :: (Gusto, Gusto) -> Helado
uncurry Rect swap -- No posee tipo
compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado
compose Just :: (b -> a) -> (b -> Maybe a)
compose uncurry (Pote Chocolate) -- No posee tipo 

-- 6
(uncurry Rect) (1.0,2.0)
(compose (uncurry Rect) swap) (1.0,2.0)
(uncurry Cucurucho) (Chocolate, Vainilla)
(compose uncurry Pote) Chocolate (Vainilla, Frutilla)
(compose Just) doble

-- 7
-- 8

-- a1
cuadrado :: Float -> Shape
cuadrado n = Rect n n

shapeNormal :: (Float -> a) -> a
shapeNormal c = c 1

chocoHelate :: (Gusto -> a) -> a
chocoHelate c = c Chocolate

string2gusto :: String -> Gusto
string2gusto "Chocolate" = Chocolate

armarHeladoCon :: (String -> Gusto) -> Helado
armarHeladoCon sg = Vasito (sg "Chocolate")

-- a2 (Falta reducir)
shapeNormal Circle :: Shape
shapeNormal cuadrado :: Shape
shapeNormal Rect :: Float -> Shape
shapeNormal Rect 2 :: Shape
shapeNormal (Rect 2) :: Shape
chocoHelate Vasito :: Helado
chocoHelate Cucurucho :: Gusto -> Helado
chocoHelate Cucurucho Sambayon :: Helado
chocoHelate (Cucurucho Sambayon) :: Helado
chocoHelate (flip Cucurucho Sambayon) :: Helado
chocoHelate (\g -> Cucurucho g Sambayon) :: Helado
chocoHelate (chocoHelate Cucurucho) :: Helado
chocoHelate Pote :: Gusto -> Gusto -> Helado
chocoHelate poteDeUnGusto :: Helado
armarHeladoCon string2gusto :: Helado
armarHeladoCon error :: Helado
esVasito (armarHeladoCon error) :: Bool

-- a3
esCuadrado :: Shape -> Bool
siempreArmaCuadradoDeMedida :: Float -> (Float -> Shape) -> Bool
siempreArmaCuadrado :: (Float -> Shape) -> Bool

-- a4
siempreArmaCuadrado cuadrado
siempreArmaCuadrado (Rect 2)
siempreArmaCuadrado (shapeNormal Rect)

-- a5
Par Bool
Par Helado
Par (Gusto -> Helado)

-- a6
DosCosas (Cucurucho Chocolate)