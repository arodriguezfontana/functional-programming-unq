id :: a -> a
id x = x

apply :: (a -> b) -> a -> b
apply f x = f x

const :: a -> b -> a
const x y = x

twice :: (a -> a) -> a -> a
twice f x = f (f x)

unflip :: (b -> a -> c) -> a -> b -> c
unflip f x y = f y x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = (f x) (g x)

compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f (g x)

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

por ppio. de ext.:
para todo , :
    ¿?

sea cualquiera, finit y totalmente definid,
quiero ver que:
    ¿?

por ppio. de ind. sobre la estructura ,
es eq. a demostrar:
    CB, =:
        ¿?
    CI, =:
        HI: ¡!
        TI: ¿?

CB.i:
CB.d:
cb demostrado.

CI.i:
CI.d:
ci demostrado.

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr (\x r -> p x || r) False xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- Bloqueo (Necesito un lema):
-- Me atasco en la demostración porque ninguna definición simple aplica,
-- y el único camino adelante requiere una nueva inducción anidada.

-- Objetivo (Cuál lema):
-- Miro lo que tengo
-- y a dónde quiero llegar
-- y generalizo esa transformación para crear la propiedad del lema.