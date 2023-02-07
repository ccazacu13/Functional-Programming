-- Subiectul 1 -----------------------------------------------------------

data Point = Pt [Int]
 deriving Show

-- Un tip de date ce reprezinta arbori binari de cautare (cu nodurile sortate):

data Arb = Empty | Node Int Arb Arb
 deriving Show

--O clasă de tipuri ToFromArb
class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

--Sa se faca o instanta a clasei ToFromArb pentru tipul Point. Inserarea in arbore se va face tinand
--cont de proprietatea arborelui de a fi sortat.

instance ToFromArb Point where
    toArb :: Point -> Arb
    toArb (Pt []) = Empty
    toArb (Pt ls) = foldl (flip insert) Empty ls        -- parcugem elementele de la stanga la dreapta cu foldl, trebuie sa inversam parametrii cu flip ai functiei de insertie
                    where 
                        insert x Empty = Node x Empty Empty     -- daca ajungem sa comparam cu un arbore gol inseram nodul
                        insert x (Node nr arb1 arb2) = if x < nr 
                                                        then
                                                            Node nr (insert x arb1) arb2    -- daca valoarea curenta este mai mica decat cea pe care o verificam acum mergem pe arborele
                                                                                            -- din stanga, altfel pe dreapta
                                                        else
                                                            Node nr arb1 (insert x arb2)



    fromArb :: Arb -> Point
    fromArb Empty = Pt []                        
    fromArb (Node nr arb1 arb2) = Pt $ nr : var1 ++ var2    -- concatenam valoarea curenta cu cea din arborii stang si drept si continuam sa coboram
                                    where 
                                        Pt var1 = fromArb arb1  --extragem lista din structura de Point
                                        Pt var2 = fromArb arb2

-- Subiectul 2 -----------------------------------------------------------

-- Functie care primeste limita inferioara si superiora si o lista si construieste lista numerelor din interval

-- varianta recursiva

getFromInterval :: Integer -> Integer -> [Integer] -> [Integer]
getFromInterval _ _ [] = [] 
getFromInterval inf sup (x : xs) = if x >= inf && x <= sup then x : getFromInterval inf sup xs      -- luam fiecare element pe rand si folosim apel recursiv unde concatenam valorile care respecta conditia
                                else getFromInterval inf sup xs

-- varianta cu monade si scriere do

getFromIntervalDo :: Integer -> Integer -> [Integer] -> [Integer]
getFromIntervalDo inf sup xs = do
                                x <- xs                         -- scoatem pe x din lista
                                if x >= inf && x <= sup         --verificam daca se afla in limite
                                    then [x]                    -- il reintroducem in monada, daca da
                                else []                         -- altfel, nu

                                                                            

-- varianta monade secventiere (>>=)

getFromIntervalSec :: Integer -> Integer -> [Integer] -> [Integer]
getFromIntervalSec inf sup xs = xs >>= \x -> if x >= inf && x <= sup then [x] else []

--varianta cu list comprehension

getFromIntervalComp :: Integer -> Integer -> [Integer] -> [Integer]
getFromIntervalComp inf sup xs = [x | x <- xs, x >= inf && x <= sup]

-- Subiectul 3 -----------------------------------------------------------

newtype ReaderWriter env a = RW {getRW :: env -> (a, String)}

-- instance Monad (ReaderWriter env) where
--     return x = RW (\_ -> (x, ""))
--     ma >>= k = RW f
--             where
--                 f env = let (va, log1) = getRW ma env
--                             (vb, log2) = getRW (k va) env
--                         in (vb, log1 ++ log2)


-- EROARE PENTRU CA CERE INSTANTA DE APPLICATIVE.