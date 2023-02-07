{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f bb

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}

--1 ---------------------------------------------------

-- Sa se scrie instantele pentru clasele functor si applicative ale tipului de date list

data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where -- definim fmap
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons a lst) = Cons (f a) (fmap f lst)

 
concatL :: List a -> List a -> List a   -- functie de concatenare intre doua elemente de tip list
lst `concatL` Nil = lst
Nil `concatL` lst = lst
(Cons x xs) `concatL` lst = Cons x (xs `concatL` lst)   -- Inlocuim ultimul Nil din prima lista cu lista 2

instance Applicative List where
    pure :: a -> List a
    pure x = Cons x Nil     -- x este adaugat intr-o instanta de lista

    (<*>) :: List (a -> b) -> List a -> List b  -- functie de liste este aplicata pe fiecare dintre elementele celei de a doua liste
    Nil <*> nrs = Nil                           -- listele partiale sunt concatenate 
    nrs <*> Nil = Nil
    (Cons f rest) <*> nrs = gen f nrs `concatL` (rest <*> nrs)
                            where
                                gen :: (a -> b) -> List a -> List b -- functie care genereaza o lista partiala formata prin aplicarea lui f
                                gen _ Nil = Nil                     -- pe fiecare dintre elementele listei
                                gen f (Cons x xs) = Cons (f x) (gen f xs)
 


f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

--2a ---------------------------------------------------

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

-- Sa se scrie functiile noEmpty si noString care valideaza un string, respectiv un intreg

noEmpty :: String -> Maybe String
noEmpty str = if null str then Nothing else Just str  --null verifica daca e o lista vida

noNegative :: Int -> Maybe Int
noNegative nr = if nr < 0 then Nothing else Just nr  --verificam daca nr e mai mic ca 0

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

--2b ---------------------------------------------------

-- Sa se scrie o functie care construieste un elem de tip Cow verificand numele, varsta si greutatea

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight = if noEmpty name /= Nothing && noNegative age /= Nothing && noNegative weight /= Nothing 
                                    then Just $ Cow name age weight
                                    else Nothing

--2c ---------------------------------------------------

-- Sa se scrie functia de la b cu fmap si <*>

cowFromString1 :: String -> Int -> Int -> Maybe Cow
cowFromString1 name age weight = Cow <$> noEmpty name <*> noNegative age <*> noNegative weight


test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

--3 ---------------------------------------------------

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

--3a ---------------------------------------------------

-- Functie validateLength care valideaza lungimea unui sir, sa fie mai mica decat numarul dat ca parametru

validateLength :: Int -> String -> Maybe String
validateLength nr str = if length str < nr then Just str else Nothing 

test31 = validateLength 5 "abc" == Just "abc"

--3b ---------------------------------------------------

-- Functiile mkName, mkAddress care transforma un sir de caractere intr-un element din tipul de date asociat, validand stringul cu functia validateLength

mkName :: String -> Maybe Name
mkName nume 
        | validateLength 25 nume == Just nume = Just $ Name nume
        | otherwise = Nothing 

mkAddress :: String -> Maybe Address
mkAddress adresa 
            | validateLength 100 adresa == Just adresa = Just $ Address adresa
            | otherwise = Nothing

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

--3c ---------------------------------------------------

-- Functia mkPerson care primeste ca argument 2 siruri de caractere si formeaza un element
-- de tip Person daca sunt validate conditiile

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = if validateLength 25 nume /= Nothing && validateLength 100 adresa /= Nothing
                        then Just $ Person (Name nume) (Address adresa)
                        else Nothing

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))

--3d ---------------------------------------------------

-- Functiile de la b si c folosind fmap si <*>

mkName1 :: String -> Maybe Name
mkName1 nume = Name <$> validateLength 25 nume

mkAddress1 :: String -> Maybe Address
mkAddress1 adresa = Address <$> validateLength 100 adresa

mkPerson1 nume adresa = Person <$> mkName1 nume <*> mkAddress1 adresa