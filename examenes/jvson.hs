-- Obtiene los valores de una lista de campos de un objeto
valuesOf :: [Field] -> (Field -> Maybe Value) -> [Value]

-- Obtiene pares (campo, valor) para los campos que existen
valuesWithFields :: [Field] -> (Field -> Maybe Value) -> [(Field,Value)]

-- Restringe un objeto a solo los campos especificados
only :: [Field] -> (Field -> Maybe Value) -> (Field -> Maybe Value)

-- Actualiza o agrega un campo con un valor en un objeto
update :: Field -> Value -> (Field -> Maybe Value) -> (Field -> Maybe Value)

-- Crea un objeto con un solo campo y valor
singleton :: Field -> Value -> (Field -> Maybe Value)

-- devuelve una lista con todos los índices presentes en la estructura, en cualquier orden
indices :: Svson i -> [i]
indices Empty = []
indices (Obj i f sv1 sv1) = i : (indices sv1 ++ indices sv2)

-- indica si un índice dado existe en la estructura. Debe aprovechar lapropiedad de BST para ser eficiente.
belongs :: Ord i => i -> Svson i -> Bool
belongs x Empty = False
belongs x (Obj i f sv1 sv2) = if i == x then True
 else if i < x then belongs x sv1
 else belongs x sv2

-- busca un índice específico y devuelve los valores de los campos solicitados para ese índice. Si el índice no existe, devuelve una lista vacía.
lookupProjecting :: Ord i => i -> [Field] -> Svson i -> [Value]
lookupProjecting x fs Empty = []
lookupProjecting x fs (Obj i f sv1 sv2) = if x == i then valuesOf fs f
 else if x
    then lookupProjecting x fs sv1
    else lookupProjecting x fs sv2

-- actualiza o inserta un campo en el objeto con el índice dado. Si el índice no existe, crea un nuevo nodo en la posición correcta del BST.
upsert :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert idx field val Empty = Obj idx (\f -> if f == field then Just val else Nothing) Empty Empty
upsert idx field val (Obj i objFunc left right) = 
    if idx == i 
    then Obj i (\f -> if f == field then Just val else objFunc f) left right
    else if idx < i 
        then Obj i objFunc (upsert idx field val left) right
        else Obj i objFunc left (upsert idx field val right)