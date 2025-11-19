-- 1. Explicita.
-- a. Inidica si la nave posee al menos un generador de campos de fuerza.
shielded :: Spaceship -> Bool 
shielded = foldSS False (\c b1 b2 -> iguales c Shield || b1 || b2)

iguales :: Component -> Component -> Bool
iguales Cargo Cargo = True
iguales Engine Engine = True
iguales Shield Shield = True
iguales Cannon Cannon = True

-- b. Indica si la nave posee al menos un cañon. 
armed :: Spaceship -> Bool
armed = foldSS False (\c b1 b2 -> iguales c Cannon || b1 || b2

-- c. Retorna el poder de propulsion de la nave.
thrust :: Spaceship -> Int
thrust = foldSS 0 (\c n1 n2 -> if iguales c Engine then 1 + n1 + n2 else n1 + n2)

-- d. Deveulve la nave resultante de despender los modulos dependientes del modulo donde se recibe el impacto (se asume que se produce el impacto). 
wreck :: Hazard -> Spaceship -> Spaceship
wreck = flip (recSS (const Plug)
                    (\c f1 s1 f2 s2 -> \(d,n,s) -> if n == 0 
    then error "no hay modulos"
    else if n == 1
        then case d of 
            Larboard -> case impactar s s1 of 
                            Plug -> Plug 
                            s1' -> Module c s1' s2
            Starboard -> case impactar s s2 of
                            Plug -> Plug 
                            s2' -> Module c s1 s2'
        else case d of 
            Larboard -> Module c (f1 (d,n-1,s)) s2
            Starboard -> Module c s1 (f2 (d,n-1,s))))

impactar :: Size -> Spaceship -> Spaceship 
impactar Small s = if shielded s then s else Plug
impactar Big s = if armed s then impactar Small s else s 
impactar Torpedo s = Plug   

-- 2. Esquema primitivo y recursivo de Spaceship.
foldSS :: b -> (Component -> b -> b -> b) -> Spaceship -> b
foldSS m p Plug = p
foldSS m p (Module c s1 s2) = m c (foldSS m p s1) (foldSS m p s2)

recSS :: b -> (Component -> Spaceship -> Spaceship -> b -> b -> b) -> Spaceship -> b
recSS m p Plug = p
recSS m p (Module c s1 s2) = m c s1 s2 (recSS p m s1) (recSS p m s2)

-- 3. Con esquemas.
-- a. Retorna la capacidad de la nave, donde cada modulo de carga aporta una unidad de capacidad.
capacity :: Spaceship -> Int
capacity = foldSS 0 (\c n m -> if iguales c Cargo then 1 + n1 + n2 else n1 + n2)

-- b. Dada una lista de naves, retorna la de capacidad maxima. 
largest :: [Spaceship] -> Spaceship 
largest = foldSS (error "no hay spacechips") (\s sr -> if capacity s >= capacity sr then s else sr) 

-- c. Dada una nave, retorna su alto y ancho (pensando el alto como la cantidad de componentes de la rama mas larga y el ancho como como la cantidad de componentes del nivel mas ancho).
dimensions :: Spaceship -> (Int, Int)
dimensions s = (altoSS s, anchoSS s)

altoSS :: Spaceship -> Int
altoSS = foldSS 0 (\_ n m -> 1 + max n m)

anchoSS :: Spaceship -> Int
anchoSS s = maximum (anchoPorNivel s) -- maximum . anchoPorNivel

anchoPorNivel :: Spaceship -> [Int]
anchoPorNivel = foldSS
    [0]
    (\c ls1 ls2 -> 1 : sumarNiveles ls1 ls2)

sumarNiveles :: [Int] -> [Int] -> [Int]
sumarNiveles = foldr 
        (\n r ms -> case ms of
                        [] -> n : r [] 
                        (m:ms') -> n + m : r ms') 
        (\ms -> ms) -- id

-- d. Simula el resultado de maniobrar una nave a traves de una serie de peligros. Si se encuentra un objeto pequeño y la nave esta escudada, no se produce impacto. Si el objeto es grande y la nave esta armada, entonces se transforma en un objeto pequeño. Si es un torpedo, no se puede evitar el impacto. 
manoeuvre :: Spaceship -> [Hazard] -> Spaceship -- [Hazard] -> (Spaceship -> Spaceship)
manoeuvre = flip (foldr
    (\h f -> \s -> f (wreck h s))
    id) 

-- e. Dadas una lista de naves y una lista de peligros, retorna la lista de naves que sobreviven los peligros, es decir, las naves con motores funcionales luego de navegar a traves de los meteoros. 
test :: [Spaceship] -> [Hazard] -> [Spaceship]
test = foldr (\s fs -> \hs -> let s' = manoeuvre s hs
                                in if thrust s' > 0 then s' : fs hs else fs hs)
            (const [])
            
-- 4. Demostrar: para todo sp, componentes (replace f sp) = map f (componentes sp)
componentes :: Spaceship -> [Component]
componentes Plug = []
componentes (Module c s1 s2) = componentes s1 ++ [c] ++ componentes s2

replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)

sea f'::Component -> Component, sp'::Spaceship, quiero ver que:
    ¿componentes (replace f' sp') = map f' (componentes sp')?

por ppio. de ind. sobre la estructura sp', es eq. a demostrar:
    cb, sp'=Plug:
        ¿componentes (replace f' Plug) = map f' (componentes Plug)?
    ci, sp'=(Module c s1 s2):
        hi1: ¡componentes (replace f' s1) = map f' (componentes s1)!
        hi2: ¡componentes (replace f' s2) = map f' (componentes s2)!
        ti: ¿componentes (replace f' (Module c s1 s2)) = map f' (componentes (Module c s1 s2))?

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

para todo f, para todo xs, para todo ys.
    ¿map f (xs ++ ys) = map f xs ++ map f ys?

sea f'::a->b, xs'::[a], ys'::[a], quiero ver que:
por ppio. de ind. sobre la estructura xs', es eq. a demostrar:
    cb, xs'=[]:
        ¿map f' ([] ++ ys') = map f' [] ++ map f ys'?
    ci, xs'=(x:xs''):
        hi: ¡map f' (xs'' ++ ys') = map f' xs'' ++ map f' ys'!
        ti: ¿map f' ((x:xs'') ++ ys) = map f' (x:xs'') ++ map f' ys'?

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