import Data.Either

-- functie care creeaza o lista cu un element replicat de x ori ex:  [3,3,3]

myReplicateDo :: Integer -> Integer -> [Integer]
myReplicateDo 1 v = return v
myReplicateDo n v = do
                    v: myReplicateDo (n-1) v

myReplicateDo1 :: Integer -> Integer -> [Integer]
myReplicateDo1 1 v = []
myReplicateDo1 n v = do
                    v: myReplicateDo (n-1) v

-- lista, suma elementelor impare

-- listapare3 :: [Maybe Integer] -> [Integer]
-- listapare3 ls = do
--                 x <- ls
--                     do
--                     y <- x
--                     if odd y then return y
--                         else Nothing

-- listapare3 :: [Maybe Int] -> [Maybe Int]
-- listapare3 ls = ls >>= \x -> x >>= \y -> if even y then return (Just y) else return ([] :: [Maybe Int])

-- [[Right 1, Right 2, Right 3], [Right 2, Right 10]]

-- listaMaybe :: [Either String Int] -> [Int]
-- listaMaybe ls = do
--                 x <- ls

newtype Sum = Sum { getSum :: Int} deriving Show

instance Semigroup Sum where
    (<>) :: Sum -> Sum -> Sum    
    Sum x <> Sum y = Sum $ x + y

instance Monoid Sum where
    mempty = Sum 0


sumElem ls = getSum $ foldMap (\(Right a) -> Sum a) ls

sumElem1 ls = foldr (\(Right a) b-> a + b) 0 ls


fct a = if isRight a then "True" else "False"

data Coord = Coord (Maybe Int) Int 
newtype Coordinates = Coordinates [Coord]
data Arb = Null | Frunza Int Int | Ramura Arb Coord Arb

class Arbore where
    toArb :: Coordinates -> Arb
    fromArb :: Arb -> Coordinates
    elimin :: Int -> Arb -> Arb
    adaug :: Coord -> Arb -> Arb
    caut :: Int -> Arb -> Arb

-- Functii care modifica elementele unei liste care sunt de tip Maybe Int, folosind monade    

func :: [Maybe Int] -> [Maybe Int]
func ls = ls >>= \x -> [(x >>= \y -> Just (y * 2))] -- in fiecare functie trebuie sa intoarcem tipul respectiv de monada
                                                    -- functia care il contine pe x trebuie sa returneze o lista, iar functia care il contine pe y un maybe

func1 :: [Maybe Int] -> [Maybe Int]                 -- pt fiecare monada avem nevoie de un do block diferit
func1 ls = do                                       -- do block pentru liste
        y <- ls
        let x = do                                  --do block pentru monade
                z <- y
                return $ z * 2;
        return x
        
func3 :: [[Int]] -> [[Int]]
func3 ls = do
            x <- ls
            y <- x
            return [y, y]

func4 :: [Maybe Int] -> Maybe Int
func4 ls = Just $ sum (do
        x <- ls                             -- Luam maybe ul din lista
        if x /= Nothing then                -- daca e nothing nu il trimitem inapoi
            let z = (\(Just m) -> m) x      -- extragem int ul Just
            in [z | even z]                 -- daca e par il trimitem in lista
        else
            [])
    -- Dupa ce am construit lista de numere pare aplicam functia sum si Just (linia 1)

func5 :: [[Int]] -> [Bool]
func5 ls = map (\x -> length x == 3 )ls