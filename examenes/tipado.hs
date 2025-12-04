-- 1. Explicita.
-- a. Dada una expresión, determina su tipo si el mismo es correcto. Las operaciones tienen tipo correcto cuando se aplican a los argumentos del tipo que corresponde, y dan como resultado el tipo que indica qué es el resultado.
typecheck :: Exp -> Maybe TType
typecheck (LNum n) = Just Number
typecheck (LText s) = Just Text
typecheck (Var v) = Just Text
typecheck (AritOp f e1 e2) = aritCheck (typecheck e1) (typecheck e2)
typecheck (RelOp f e1 e2) = relCheck (typecheck e1) (typecheck e2)
typecheck (Text2TextOp f e1) = t2tCheck (typecheck e) 
typecheck (Text2NumOp f e1) = t2nCheck (typecheck e)

aritCheck :: Maybe TType -> Maybe TType -> Maybe TType
aritCheck (Just Number) (Just Number) = Just Number 
aritCheck _ _ = Nothing

relCheck :: Maybe TType -> Maybe TType -> Maybe TType
relCheck (Just Number) (Just Number) = Just Boolean 
relCheck _ _ = Nothing

t2tCheck :: Maybe TType -> Maybe TType
t2tCheck (Just Text) = Just Text 
t2tCheck _ = Nothing

t2nCheck :: Maybe TType -> Maybe TType
t2nCheck (Just Text) = Just Number 
t2nCheck _ = Nothing

-- b. Dada una expresión y un valor para cada variable (como funcion total), evalua la expresion al valor que corresponde. Si la expresion no tiene tipo valido, da el valor TypeError, de lo contrario, devuelve el resultado correspondiente como elemento de tipo Value.
evalExp :: Exp -> (Variable -> String) -> Value
evalExp (LNum n) _ = VInt n
evalExp (LText s) _ = VText s
evalExp (Var v) f = VText (f v)
evalExp (AritOp g e1 e2) f = aritVal g (evalExp e1 f) (evalExp e2 f)
evalExp (RelOp g e1 e2) f = relVal g (evalExp e1 f) (evalExp e2 f)
evalExp (Text2TextOp g e1) f = t2tVal g (evalExp e f)
evalExp (Text2NumOp g e1) f = t2nVal g (evalExp e f)

aritVal :: (Int -> Int -> Int) -> Value -> Value -> Value
aritVal g (VInt n) (VInt m) = VInt (g n m)
aritVal _ _ _ = TypeError

relVal :: (Int -> Int -> Bool) -> Value -> Value -> Value
relVal g (VInt n) (VInt m) = VBool (g n m)
relVal _ _ _ = TypeError

t2tVal :: (String -> String) -> Value -> Value 
t2tVal g (VText s) = VText (g s)
t2tVal _ _ = TypeError

t2nVal :: (String -> String) -> Value -> Value 
t2nVal g (VText s) = VInt (g s)
t2nVal _ _ = TypeError

-- 2. Demostrar: para todo env. typecheck = flip evalExp env.
por ppio. de ext.
    ¿para todo env. para todo exp. typecheck exp = (tvalue . flip evalExp env) exp?

sean f::(Variable -> String), e::Exp, quiero ver que, por ppio. de ind. estructural sobre e:
-- Se deben plantear todos los casos pero desarrollar solamente Var y RelOp.
    cb, e=(Var v)
        ¿typecheck (Var v) = (tvalue . flip evalExp f) (Var v)?
    
    ci, e=(RelOp g e1 e2)
        h1: ¡typecheck e1 = (tvalue . flip evalExp f) e1!
        h2: ¡typecheck e2 = (tvalue . flip evalExp f) e2!
        t: ¿typecheck (RelOp g e1 e2) = (tvalue . flip evalExp f) (RelOp g e1 e2)?

cbi
typecheck (Var v)
-- typecheck.3
Just Text

cbd
(tvalue . flip evalExp f) (Var v)
-- Descomposición de (.) y flip
tvalue (evalExp (Var v) f)
-- evalExp.3
tvalue (VText f v)
-- tvalue.3
Just Text

cb demostrado.

cii
typecheck (RelOp g e1 e2)
-- typecheck.5
relCheck (typecheck e1) (typecheck e2) 
-- h1 y h2
relCheck (tvalue (evalExp e1 f)) (tvalue (evalExp e2 f))
-- lema
tvalue (relVal g (evalExp e1 f) (evalExp e2 f))

cid
(tvalue . flip evalExp f) (RelOp g e1 e2)
-- Descomposición de (.) y flip
tvalue (evalExp (RelOp g e1 e2) f)
-- evalExp.5
tvalue (relVal g (evalExp e1 f) (evalExp e2 f))

-- lema
para todo v1'. para todo v2' relCheck (tvalue v1') (tvalue v2') = tvalue (relVal g v1' v2')

sean v1:Value, v2:Valu, quiero ver que, por casos sobre v1 y v2:
    c1, v1=VInt n, v2=VInt m
    c2, v1/=VInt n, v2/=VInt m

c1i
relCheck (tvalue (VInt n)) (tvalue (VInt m)
-- tvalue.1
relCheck (Just Number) (Just Number)
-- relCheck.1
Just Boolean

c1d
tvalue (relVal g (VInt n) (VInt m))
-- relVal.1
tvalue (vBool (g n m))
-- tvalue.2
Just Boolean

c1 demostrado.

c2i
relCheck (tvalue (VBool b1)) (tvalue (VBool b2)
-- tvalue.2
relCheck (Just Boolean) (Just Boolean)
-- relCheck.2
Nothing

c2d
tvalue (relVal g (VBool b1) (VBool b2))
-- relVal.2
tvalue (TypeError)
-- tvalue.4
Nothing

c2 demostrado.

-- 3. Esquema recursivo y primitivo de Exp.
foldE :: 
    (Int -> b) ->
    (String -> b) ->
    (Variable -> b) ->
    ((Int -> Int -> Int) -> b -> b -> b) ->
    ((Int -> Int -> Bool) -> b -> b -> b) ->
    ((String -> String) -> b -> b) ->
    ((String -> Int) -> b -> b) ->
    Exp ->
    b
foldE fn ft fv fa fr ftt ftn (LNum n) = fn n
foldE fn ft fv fa fr ftt ftn (LText s) = ft s
foldE fn ft fv fa fr ftt ftn (Var v) = fv v
foldE fn ft fv fa fr ftt ftn (AritOp f e1 e2) = fa f (foldE fn ft fv fa fr ftt ftn e1) (foldE fn ft fv fa fr ftt ftn e2)
foldE fn ft fv fa fr ftt ftn (RelOp f e1 e2) = fr f (foldE fn ft fv fa fr ftt ftn e1) (foldE fn ft fv fa fr ftt ftn e2)
foldE fn ft fv fa fr ftt ftn (Text2TextOp f e1) = ftt f (foldE fn ft fv fa fr ftt ftn e)
foldE fn ft fv fa fr ftt ftn (Text2NumOp f e1) = ftn f (foldE fn ft fv fa fr ftt ftn e)

recE :: 
    (Int -> b) ->
    (String -> b) ->
    (Variable -> b) ->
    ((Int -> Int -> Int) -> Exp -> Exp -> b -> b -> b) ->
    ((Int -> Int -> Bool) -> Exp -> Exp -> b -> b -> b) ->
    ((String -> String) -> Exp -> b -> b) ->
    ((String -> Int) -> Exp -> b -> b) ->
    Exp ->
    b
recE fn ft fv fa fr ftt ftn (LNum n) = fn n
recE fn ft fv fa fr ftt ftn (LText s) = ft s
recE fn ft fv fa fr ftt ftn (Var v) = fv v
recE fn ft fv fa fr ftt ftn (AritOp f e1 e2) = fa f e1 e2 (recE fn ft fv fa fr ftt ftn e1) (recE fn ft fv fa fr ftt ftn e2)
recE fn ft fv fa fr ftt ftn (RelOp f e1 e2) = fr f e1 e2 (recE fn ft fv fa fr ftt ftn e1) (recE fn ft fv fa fr ftt ftn e2)
recE fn ft fv fa fr ftt ftn (Text2TextOp f e) = ftt f e (recE fn ft fv fa fr ftt ftn e)
recE fn ft fv fa fr ftt ftn (Text2NumOp f e) = ftn f e (recE fn ft fv fa fr ftt ftn e)

-- 4. Con esquemas.
typecheck :: Exp -> Maybe TType
typecheck = foldE
    (/_ -> Just Number)
    (/_ -> Just Text)
    (/_ -> Just Text)
    (/_ m1 m2 -> aritCheck m1 m2)
    (/_ m1 m2 -> relCheck m1 m2)
    (/_ m -> t2tCheck m)
    (/_ m -> t2nCheck m)

evalExp :: Exp -> (Variable -> String) -> Value
evalExp = foldE
    (/n f -> VInt n)
    (/s f -> VText n)
    (/v f -> VText (f v))
    (/g hr1 hr2 f -> aritVal g (hr1 f) (hr2 f))
    (/g hr1 hr2 f -> relVal g (hr1 f) (hr2 f))
    (/g hr f -> t2tVal g (hr f))
    (/g hr f -> t2nVal g (hr f))

-- 5 Con esquemas.
foldC :: a ->
    (Variable -> String -> a) ->
    (Variable -> a) ->
    (a -> a -> a) ->
    (Exp -> a -> a -> a) ->
    Cmd ->
    a
foldC fsk fst fp fsq fi c = case c of
    Skip -> fsk
    StoreInput v s -> fst v s
    PrintVar v -> fp v
    Seq c1 c2 -> fsq (re c1) (re c2)
    If e c1 c2 -> fi e (re c1) (re c2)
  where
    re = foldCmd fsk fst fp fsq fi

-- a. Describe un comando equivalente al dado, pero que no utilice Skip en ninguna secuencia, y que elimine aquellas alternativas cuyas condiciones tienen comparaciones entre 2 literales.
simplifyCmd :: Cmd -> Cmd
simplifyCmd = foldC
    Skip
    StoreInput
    PrintVar
    seqSimpl
    ifSimpl

simplSeq :: Cmd -> Cmd -> Cmd
simplSeq Skip c2 = c2
simplSeq c1 Skip = c1
simplSeq c1 c2 = Seq c1 c2

simplIf :: Exp -> Cmd -> Cmd -> Cmd
simplIf (RelOp op (LNum n1) (LNum n2)) c1 c2 = if op n1 n2 then c1 else c2
simplIf e c1 c2 = If e c1 c2

-- bonus. Ejecuta el comando dado. En la ejecución: Los comandos de asignación modifican la memoria, los comandos de impresión modifican la salida y los comandos de secuencia y alternativa se comportan como los usuales en un lenguaje de programación secuencial.
evalCmd :: Cmd -> State -> Maybe State
evalCmd = foldC
            evalSkip
            evalStore
            evalPrint
            evalSeq
            evalIf

evalSkip :: State -> Maybe State
evalSkip s = Just s

evalStore :: Variable -> String -> State -> Maybe State
evalStore v s (S mem out) = 
    let newMem = \var -> if var == v then s else mem var 
    in Just (S newMem out)

evalPrint :: Variable -> State -> Maybe State
evalPrint v (S mem out) = Just (S mem (out ++ [mem v]))

evalSeq :: (State -> Maybe State) -> (State -> Maybe State) -> State -> Maybe State
evalSeq f1 f2 s = case f1 s of
    Nothing -> Nothing
    Just s' -> f2 s

evalIf :: Exp -> (State -> Maybe State) -> (State -> Maybe State) -> State -> Maybe State
evalIf e fTrue fFalse s@(S mem out) = 
    case evalExp e mem of
        VBool True -> fTrue s
        VBool False -> fFalse s
        _ -> Nothing