data Component = Cargo | Engine | Shield | Cannon 
    deriving Show
data Spaceship = Module Component Spaceship Spaceship | Plug
    deriving Show

data Direction = Larboard | Starboard
    deriving Show 
data Size = Small | Big | Torpedo
    deriving Show 
type Hazard = (Direction, Int, Size)


-- 1: Definir por recursion explicita las siguientes funciones.

-- a. Inidica si la nave posee al menos un generador de campos de fuerza. 
shielded :: Spaceship -> Bool 

-- b. Indica si la nave posee al menos un cañon. 
armed :: Spaceship -> Bool

-- c. Redorna el poder de propulsion de la nave. 
thrust :: Spaceship -> Int

-- d. Deveulve la nave resultante de despender los modulos dependientes del modulo donde se recibe el impacto (se asume que se produce el impacto). 
wreck :: Hazard -> Spaceship -> Spaceship

