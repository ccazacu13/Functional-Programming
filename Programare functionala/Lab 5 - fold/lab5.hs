
--1. ---------------------------------------------------

-- Functie care calculeaza suma patratelor numerelor impare dintr-o lista data ca parametru

sumaPatrate :: [Int] -> Int
sumaPatrate ls = foldr (\a b -> if odd a then b + a^2 else b) 0 ls  -- daca un numar din lista este impar adaugam la elementul neutru patratul lui

--2. ---------------------------------------------------

-- Functie care verifica ca toate elementele dintr-o lista sunt True

verifica :: [Bool] -> Bool
verifica ls = foldr (\a b -> a && b) True ls -- folosim operatie de && intre elementele noastre si elementul neutru

--3. ---------------------------------------------------

-- Functie care verifica daca toate elemente unei liste satisfac proprietatea data ca parametru

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies pred ls = foldr (\a b -> pred a && b) True ls

--4. ---------------------------------------------------

-- Functie care verifica daca exista un element intr-o lista care satisface o proprietate data

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies pred ls = foldr (\a b -> pred a || b) False ls  

--5. ---------------------------------------------------

-- Definiti functiile map si filter folosind foldr

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f ls = foldr (\a b -> f a : b) [] ls -- aplicam f pe elementele listei si le atasam la lista finala

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr pred = foldr (\a b -> if pred a then a : b else b) [] -- daca elementele respecta proprietatea le concatenam la elementul neutru (lista finala)

--6. ---------------------------------------------------

-- Functie care transforma o lista de cifre intr-un numar, folosind foldl

listToInt :: [Integer] -> Integer
listToInt ls = foldl (\b a -> b * 10 + a) 0 ls  -- foldl primeste o functie care are elementul neutru pe prima pozitie si elementul listei pe a doua

--7. ---------------------------------------------------

-- Functie care elimina un caracter dintr-un sir de caractere

rmChar :: Char -> [Char] -> [Char]
rmChar c = foldr (\a b -> if c /= a then a : b else b) "" -- daca caracterul din sir este diferit de char ul primit atunci il concatenam

-- Functie recursiva care elemina toate caracterele din al doilea argument care se gasesc in primul, folosind rmChar

rmCharsRec :: String -> String -> String
rmCharsRec "" str = str
rmCharsRec (c : cs) str2 = rmCharsRec cs $ rmChar c str2  -- eliminam pe rand caracterele din str1 in str2

-- Functie cu foldr care este echivalenta cu cea de mai sus

rmCharsFold :: String -> String -> String
rmCharsFold str1 str2 = foldr rmChar str2 str1   -- folosim stringul 2 ca element neutru si scoatem din el pe rand literele din primul string cu foldr si rmChar




