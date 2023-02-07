import GHCi.Message (SerializableException)
data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

--1. ---------------------------------------------------

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16


-- Instanta a clasei Show pentru tipul de date Expr

instance Show Expr where
    show :: Expr -> String
    show (Const int) = "Const " ++ show int
    show (exp1 :+: exp2) = "(" ++ show exp1 ++ " + " ++ show exp2 ++ ")"
    show (exp1 :*: exp2) = "(" ++ show exp1 ++ " * " ++ show exp2 ++ ")"

-- Functie care evalueaza valoarea unei expresii

evalExp :: Expr -> Int  --pattern matching pe fiecare constructor de date al tipului Expr
evalExp (Const a) = a
evalExp (exp1 :+: exp2) = evalExp exp1 + evalExp exp2
evalExp (exp1 :*: exp2) = evalExp exp1 * evalExp exp2  

-- Functie care evalueaza o expresie modelata sub forma de arbore, determinand valoarea acesteia

evalArb :: Tree -> Int
evalArb (Lf a) = a
evalArb (Node Add tree1 tree2) = evalArb tree1 + evalArb tree2 
evalArb (Node Mult tree1 tree2) = evalArb tree1 * evalArb tree2 


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

-- Functie care transforma o expresie in aborele corespunzator

expToArb :: Expr -> Tree
expToArb (Const a) = Lf a
expToArb (exp1 :+: exp2) = Node Add (expToArb exp1) (expToArb exp2)
expToArb (exp1 :*: exp2) = Node Mult (expToArb exp1) (expToArb exp2)

--2. ---------------------------------------------------

-- Adaugati definitii implicite pentru functiile keys, values, fromList


class Collection c where
  empty :: c key value                                      --crearea unei colectii vide
  singleton :: key -> value -> c key value                  --crearea unei colectii cu un element
  insert :: Ord key => key -> value -> c key value -> c key value -- adaugarea/actualizarea unui element intr-o colectie       
  clookup :: Ord key => key -> c key value -> Maybe value   --cautarea unui element intr-o colectie
  delete :: Ord key => key -> c key value -> c key value    --stergerea (marcarea ca sters a) unui element dintr-o colectie
  keys :: c key value -> [key]                              --obtinerea listei cheilor
  values :: c key value -> [value]                          --obtinerea listei valorilor
  toList :: c key value -> [(key, value)]                   --obtinerea listei elementelor
  fromList :: Ord key => [(key,value)] -> c key value 

  keys col = map fst $ toList col   -- obtinem lista de elemente (key, valoare) si luam primul element cu first

  values col = map snd $ toList col -- obtinem valorile folosind snd din valorile (key, value)

  fromList [] = empty
  fromList ((key, value) : xs) = insert key value (fromList xs)  -- adaugam elementul unu cate unu folosind insert

--2.2 ---------------------------------------------------

-- Definiti o instanta pentru Pairlist a clasei collection

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }
    deriving Show

instance Collection PairList where
    empty :: PairList a b
    empty = PairList []     -- contructor de date + lista vida

    singleton :: key -> value -> PairList key value
    singleton key value = PairList [(key, value)]   -- constructor de date + tuplu cheie valoare

    insert :: Ord key => key -> value -> PairList key value -> PairList key value
    insert key value (PairList ls) = PairList $ (key, value) : ls

    clookup :: Ord key => key -> PairList key value  -> Maybe value
    clookup _ (PairList []) = Nothing
    clookup key (PairList ls) = if not $ null val then Just . snd $ head val else Nothing   -- daca lista nu e vida atunci returnam Just valoare, altfel nothing
                                where val = [(key, value) | (k, value) <- ls, k == key] --contruim lista care contine elemente care au cheia data

    delete :: Ord key => key -> PairList key value -> PairList key value
    delete key (PairList ls) = PairList [(k, value) | (k, value) <- ls, k /= key]   -- generez lista fara elementele cu cheia data

    toList = getPairList    -- folosim functia care ia lista din PairList, este data in definitie
                                
--2.3 ---------------------------------------------------

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance (Show key, Show value) => Show (SearchTree key value) where
    show :: SearchTree key value -> String
    show Empty = "(Empty)"
    show (BNode left key val right) = "(BNode" ++ " " ++ show left ++ " (" ++ show key ++ ", " ++ show val ++ ") " ++ show right ++ ")"

instance Collection SearchTree where
    empty :: SearchTree key value
    empty = Empty   -- returnam un arbore gol

    singleton :: key -> value -> SearchTree key value 
    singleton key value = BNode Empty key (Just value) Empty    -- returnam un nod in care introcem key si just value

    insert :: Ord key => key -> value -> SearchTree key value -> SearchTree key value
    insert key value Empty = singleton key value
    insert key value (BNode left k val right) = if key < k then BNode (insert key value left) k val right else BNode left k val $ insert key value right   -- cautam pe ramurile arborelui pana gasim un nod empty si adaugam elementul

    clookup :: Ord key => key -> SearchTree key value -> Maybe value
    clookup key Empty = Nothing
    clookup key (BNode left k val right)
                                    | key == k = val                    -- daca cheia din arbore = cea data atunci returnam valoarea
                                    | key < k = clookup key left        -- altfel mergem pe una dintre ramuri
                                    | key > k = clookup key right

    delete :: Ord key => key -> SearchTree key value -> SearchTree key value
    delete key Empty = Empty
    delete key (BNode left k val right) 
                                    | key == k = BNode left k Nothing right
                                    | key < k = BNode (delete key left) k val right
                                    | key > k = BNode left k val (delete key right)

    toList :: SearchTree key value -> [(key, value)]
    toList Empty = []
    toList (BNode left k (Just val) right) = [(k, val)] ++ toList left  ++ toList right
    toList (BNode left k Nothing right) = toList left  ++ toList right