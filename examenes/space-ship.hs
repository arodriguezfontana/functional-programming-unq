data Component = Cargo | Engine | Shield | Cannon 
data Spaceship = Module Component Spaceship Spaceship | Plug

data Direction = Larboard | Starboard
data Size = Small | Big | Torpedo 
type Hazard = (Direction, Int, Size)

-- 1. Recursion explicita.

-- a. Inidica si la nave posee al menos un generador de campos de fuerza.
shielded :: Spaceship -> Bool 
shielded Plug = Flase
shielded (Module c s1 s2) = iguales c Shield || shielded s1 || shielded s2

iguales :: Component -> Component -> Bool
iguales Cargo Cargo = True
iguales Engine Engine = True
iguales Shield Shield = True
iguales Cannon Cannon = True

-- b. Indica si la nave posee al menos un cañon. 
armed :: Spaceship -> Bool
armed Plug = Flase
armed (Module c s1 s2) = iguales c Cannon || armed s1 || armed s2

-- c. Retorna el poder de propulsion de la nave.
thrust :: Spaceship -> Int
thrust Plug = 0
thrust (Module c Plug Plug) = unoSi (iguales c Engine)
thrust (Module c s1 s2) = thrust s1 + thrust s2

-- d. Deveulve la nave resultante de despender los modulos dependientes del modulo donde se recibe el impacto (se asume que se produce el impacto). 
wreck :: Hazard -> Spaceship -> Spaceship
wreck _ Plug = Plug
wreck (d,1,s) _ = Plug
wreck (d,n,s) (Module c s1 s2) = case d of
    Larboard -> Module c (wreck (d,n-1,s) s1) s2
    Starboard -> Module c s1 (wreck (d,n-1,s) s2)

-- 2. Esquema primitivo y recursivo de Spaceship.
foldSS :: (Component -> b -> b -> b) -> b -> Spaceship -> b
foldSS _ p Plug = p
foldSS m p (Module c s1 s2) = m c (foldSS p m s1) (foldSS p m s2)

recSS :: (Component -> Spaceship -> Spaceship -> b -> b -> b) -> b -> Spaceship -> b
recSS _ p Plug = p
recSS m p (Module c s1 s2) = m c s1 s2 (recSS p m s1) (recSS p m s2)

-- 3. Sin recursion explicita.

-- a. Retorna la capacidad de la nave, donde cada modulo de carga aporta una unidad de capacidad.
capacity :: Spaceship -> Int
capacity = foldSS (\c n m -> unoSi (iguales c Cargo) + n + m) 0

-- b. Dada una lista de naves, retorna la de capacidad maxima. 
largest :: [Spaceship] -> Spaceship 
largest = foldSS (\s s' -> if capacity s >= capacity s'
    then s else s') Plug

-- c. Dada una nave, retorna su alto y ancho (pensando el alto como la cantidad de componentes de la rama mas larga y el ancho como como la cantidad de componentes del nivel mas ancho).
dimensions :: Spaceship -> (Int, Int)
dimensions s = (altoSS s, anchoSS s)

altoSS :: Spaceship -> Int
altoSS = foldSS (\_ n m -> 1 + max n m) 0

anchoSS :: Spaceship -> Int
anchoSS = 

-- d. Simula el resultado de maniobrar una nave a traves de una serie de peligros.
-- Si se encuentra un objeto pequeño y la nave esta escudada, no se produce impacto. 
-- Si el objeto es grande y la nave esta armada, entonces se transforma en un objeto pequeño. 
-- Si es un torpedo, no se puede evitar el impacto. 
manoeuvre :: Spaceship -> [Hazard] -> Spaceship

-- e. Dadas una lista de naves y una lista de peligros, retorna la lista de naves que sobreviven los peligros, es decir, las naves con motores funcionales luego de navegar a traves de los meteoros. 
test :: [Spaceship] -> [Hazard] -> [Spaceship]

-- 4. Demostrar: para todo sp::Spaceship. 
-- componentes (replace f sp) = map f (componentes sp)
componentes :: Spaceship -> [Component]
componentes Plug = []
componentes (Module c s1 s2) = componentes s1 ++ [c] ++ componentes s2

replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)

sea
f'::Component -> Component
sp'::Spaceship
quiero ver que:
    ¿componentes (replace f' sp') = map f' (componentes sp')?

por ppio. de ind. sobre la estructura sp', es eq. a demostrar:
    CB, sp'=Plug:
        ¿componentes (replace f' Plug) = map f' (componentes Plug)?
    CI, sp'=(Module c s1 s2):
        HI1: ¡componentes (replace f' s1) = map f' (componentes s1)!
        HI2: ¡componentes (replace f' s2) = map f' (componentes s2)!
        TI: ¿componentes (replace f' (Module c s1 s2)) = map f' (componentes (Module c s1 s2))?

cbi:
componentes (replace f' Plug)
-- replace.1
componentes Plug
-- componentes.1
[]

cbd:
map f' (componentes Plug)
-- componentes.1
map f' []
-- map.1
[]

cb demostrado.

cii:
componentes (replace f' (Module c s1 s2))
-- replace.2
componentes (Module (f' c) (replace f' s1) (replace f' s2))
-- componentes.2
componentes (replace f' s1) ++ [f' c] ++ componentes (replace f' s2)
-- h1
map f' (componentes s1) ++ [f' c] ++ componentes (replace f' s2)
-- h2
map f' (componentes s1) ++ [f' c] ++ map f' (componentes s2)

cid:
map f' (componentes (Module c s1 s2))
-- componentes.2
map f' (componentes s1 ++ [c] ++ componentes s2)
-- lema: map f (xs ++ ys) = map f xs ++ map f ys
map f' (componentes s1) ++ map f' ([c] ++ componentes s2)
-- lema: map f (xs ++ ys) = map f xs ++ map f ys
map f' (componentes s1) ++ map f' [c] ++ map f' (componentes s2)
-- map.2
map f' (componentes s1) ++ f' c : [] ++ map f' (componentes s2)
-- :.2
map f' (componentes s1) ++ [f' c] ++ map f' (componentes s2)

ci demostrado.

lema: map f (xs ++ ys) = map f xs ++ map f ys

sea
f'::a->b
xs'::[a]
ys'::[a]
quiero ver que:

por ppio. de ind. sobre la estructura xs', es eq. a demostrar:
    CB, xs'=[]:
        ¿map f' ([] ++ ys') = map f' [] ++ map f ys'?
    CI, xs'=(x:xs''):
        HI: ¡map f' (xs'' ++ ys') = map f' xs'' ++ map f' ys'!
        TI: ¿map f' ((x:xs'') ++ ys) = map f' (x:xs'') ++ map f' ys'?

cbi:
map f' ([] ++ ys')
-- ++.1
map f' ys'

cbd:
map f' [] ++ map f' ys'
-- map.1
[] ++ map f' ys'
-- ++.1
map f' ys'

cb demostrado.

cii:
map f' ((x:xs'') ++ ys')
-- ++.2
map f' (x : xs'' ++ ys')
-- map.2
f' x : (map f' (xs'' ++ ys'))
-- hi
f' x : map f' xs'' ++ map f' ys'

cid:
map f' (x:xs'') ++ map f' ys'
-- map.2
f' x : map f' xs'' ++ map f' ys'

ci demostrado.