-- import Data.Monoid       UNII DINTRE MONOIZII IMPLEMENTATI POT APAREA SI IN DATA.MONOID

--1 ---------------------------------------------------

-- Implementati functiile folosding foldr sau foldMap din clasa Foldable

-- Functie care determina daca un element se afla intr-o colectie

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x ls = foldr (\a b -> x == a || b) False ls   -- initial valoarea e fals si daca gasim nr in lista vom transforma elementul neutru in true cu operatorul || 

-- Alternativa foldMap

newtype Any = Any {getAny :: Bool} deriving Show

instance Semigroup Any where            -- pentru instanta de monoid avem nevoie de instanta de semigroup in care este implementata operatia asociativa
    (<>) :: Any -> Any -> Any
    x <> y = Any $ getAny x || getAny y

instance Monoid Any where       -- facem o instanta de monoid care are ca element neutru Any False, pentru ca nu afecteaza operatia asociativa ||
    mempty :: Any
    mempty = Any False

elem2 :: (Eq a, Foldable t) => a -> t a -> Bool
elem2 x ls = getAny $ foldMap (\a -> Any $ x == a) ls       --foldMap ia fiecare element din lista, il introduce in monoid si apoi aplica operatia asociativa intre elementele introduse in monoid
                                                            -- foldmap (\a -> Any True) [1,2] => Any True <> Any True = Any (True || True) = Any True 

-----------------------------------

-- Functie care determina daca o colectie este vida

null1 :: (Foldable t) => t a -> Bool
null1 ls =  foldr (\_ _ -> False) True ls           -- daca exista un element in lista vom intra in functie si rezultatul final devine false, altfel vom returna elementul neutru initial

-- Alternativa foldMAp

null2 :: Foldable t => t a -> Bool
null2 ls = not $ getAny (foldMap (\a -> Any True) ls)   -- daca apare un element in lista vom transforma valoarea finala in true si apoi vom nega rezultatul pentru a simula functia null implementata in prologue

-----------------------------------

-- Functie care determina lungimea unei colectii

length1 :: (Foldable t) => t a -> Int
length1 ls = foldr (\a b -> b + 1) 0 ls     -- pt fiecare element adaug + 1 la rezultatul final, care este lumea listei

-- Alternativa foldMap

newtype Sum = Sum {getSum :: Int} deriving Show      -- implementam monoid ul sum

instance Semigroup Sum where            -- operatia asociativa este suma, iar monoidul contine doar un numar intreg care poate adunat
    (<>) :: Sum -> Sum -> Sum
    Sum x <> Sum y = Sum $ x + y

instance Monoid Sum where               -- elementul neutru este Sum 0, pt ca nu va afecta operatia asociativa
    mempty :: Sum
    mempty = Sum 0

length2 :: Foldable t => t a -> Int
length2 ls = getSum $ foldMap (\a -> Sum 1) ls      -- pt fiecare element din lista vom returna Sum 1 care vor fi adunate cu operatia asocitiva pentru a determina lungimea listei

-----------------------------------

-- Functie care converteste o colectie intr-o lista

toList1 :: (Foldable t) => t a -> [a]
toList1 ls = foldr (:) [] ls            -- folosim constructorul de lista si adaugam elementele pe rand in lista initial vida

-- Alternativa foldMap

newtype Concat a = Concat {getConcat :: [a]} deriving Show

instance Semigroup  (Concat a) where            -- operatie asociativa care reprezinta concatenarea pe liste
    (<>) :: Concat a -> Concat a -> Concat a
    Concat l1 <> Concat l2 = Concat $ l1 ++ l2

instance Monoid (Concat a) where
    mempty :: Concat a
    mempty = Concat []                          -- elementul neutru este lista vida

toList2 :: Foldable t => t a -> [a]
toList2 ls = getConcat $ foldMap (\a -> Concat [a]) ls      -- luam fiecare element din lista si le concatenam

-----------------------------------

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 ls = foldr (<>) mempty ls                     -- folosim operatia asociativa ca functie si elementul neutru al operatiei

-- Alternativa foldMap

fold2 :: (Foldable t, Monoid m) => t m -> m
fold2 ls = foldMap id ls                            -- folosim functia id care intoarce elementul din lista , deoarece ele sunt deja in monoizi => id (Monoid c) = Monoid c


--2 ---------------------------------------------------

-- Scrieti instante de foldable pentru tipurile de mai jos implementand foldMap

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap :: Monoid m => (b -> m) -> Constant a b -> m      -- luam b-ul din constant si aplicam pe el functia care il trimite in monoid
    foldMap f (Constant b) = f b

-----------------------------------

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap :: Monoid m => (b -> m) -> Two a b -> m    
    foldMap f (Two a b) = f b

-----------------------------------

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap :: Monoid m => (c -> m) -> Three a b c -> m
    foldMap f (Three a b c) = f c

-----------------------------------

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap :: Monoid m => (b -> m) -> Three' a b -> m
    foldMap f (Three' a b c) = f b <> f c                   --folosim operatia asociativa pentru monoidul nostru pentru a combina valorile 

-----------------------------------

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap :: Monoid m => (b -> m) -> Four' a b -> m
    foldMap f (Four' a b c d) = f b <> f c <> f d           -- folosim operatia asociativa pentru toate tipurile b din Four'

-----------------------------------

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable GoatLord where
    foldMap :: Monoid m => (a -> m) -> GoatLord a -> m
    foldMap f NoGoat = mempty                               -- daca nu avem valoare il trimitem in elementul neutru
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats a b c) = foldMap f a <> foldMap f b <> foldMap f c     --folosim recursiv foldMap pentru ca: a b c si sunt de tip GoatLord 

--exemplu : foldMap (\a -> Sum a) (MoreGoats (OneGoat 1) (OneGoat 2) (OneGoat 3)) == Sum {getSum = 6}
