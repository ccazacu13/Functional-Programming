import Data.Char

--1. ---------------------------------------------------

-- Functie nrVocale care calculeaza nr total de vocale din cuvintele palindrom
-- Hints: reverse pentru a afla daca sirul e palindrom si elem pt gasirea unui element in sir

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (str : strs) = if str == reverse str then  -- verific daca sirul e palindrom
                            adunVocale str + nrVocale strs  
                        else
                            nrVocale strs
                        where
                             adunVocale [] = 0  -- functie care imi calculeaza nr de vocale din string
                             adunVocale (x : xs) = if x `elem` "aeiouAEIOU" then  -- daca caracterul este vocala il adaug la suma
                                                    1 + adunVocale xs
                                                else 
                                                    adunVocale xs 

--2. ---------------------------------------------------

-- Functie care primeste un nr ca parametru si il adauga dupa fiecare nr par

f :: Integer -> [Integer] -> [Integer]
f nr [] = []
f nr (x : xs) =  if even x then x : nr : f nr xs    -- este construita lista de la dreapta la stanga si adaugam nr daca x este par
                    else x : f nr xs -- altfel lasam lista identica

--3. ---------------------------------------------------

-- Functie care are ca parametru un numar intreg si determina lista de divizori

divizori :: Int -> [Int]
divizori nr = [ x | x <- [1..nr], nr `mod` x == 0]  -- parcugem nr de la 1 la nr si le adaugam in lista pe cele care au restul impartirii 0

--4. ---------------------------------------------------

-- Functie care are ca parametru o lista de nr intregi si calculeaza lista listelor de divizori

listaDiv :: [Int] -> [[Int]]
listaDiv ls = [divizori x | x <-ls]

--Alternative 

listaDiv1 :: [Int] -> [[Int]]
listaDiv1 [] = []
listaDiv1 (x : xs) = divizori x : listaDiv1 xs

--5. ---------------------------------------------------

-- Functie care primeste limita inferioara si cea superioara si filtreaza o lista avand elemntele care se afla in interval

inInterval :: Integer -> Integer -> [Integer] -> [Integer]
inInterval inf sup ls = [x | x <- ls, x >= inf && x <= sup]

--6. ---------------------------------------------------

-- Functie care numara cate numere sunt strict pozitive dintr-o lista data ca argument

-- functie recursiva

pozitiveRec :: [Integer] -> Integer
pozitiveRec [] = 0
pozitiveRec (x : xs) = if x > 0 then 1 + pozitiveRec xs
                            else pozitiveRec xs

-- functie cu comprehensiune
pozitive :: [Integer] -> Int
pozitive ls = length [x | x <- ls, x > 0]   -- calculam lungimea listei care contine elemente pozitive din lista initiala

--7. ---------------------------------------------------

-- Functie care calculeaza lista pozitiilor numerelor impare

-- functie recursiva

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare ls = pozitiiAuxiliar 1 ls -- folosim o functie auxiliara care are pe prima pozitie, pozitia curenta la care ne aflam in lista
                where
                    pozitiiAuxiliar _ [] = []   
                    pozitiiAuxiliar poz (x : xs) = if odd x then poz : pozitiiAuxiliar (poz + 1) xs -- daca numarul este impar atunci adaugam pozitia lui la lista finala
                                                        else pozitiiAuxiliar (poz + 1) xs

-- functie cu comprehensiune

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp ls =  [i | (x, i) <- zip ls [1..], odd x]     -- le atasam numerelor un index si daca sunt impare adaugam indexul in lista, ex: zip [1,4,5,6] [1..] => [(1,1),(4,2),(5,3),(6,4)]

--8. ---------------------------------------------------

-- Functie care calculeaza produsul cifrelor care se afla intr-un sir de caractere

-- functie recursiva

multDigits :: String -> Int
multDigits [] = 0
multDigits (c : cs) = if isDigit c then digitToInt c * multDigits cs    -- functie care verifica daca un caracter este cifra (isDigit), digitToInt face conversia de la caracter la cifra, trebuie folosit : import Data.Char 
                        else multDigits cs

-- functie cu comprehensiune

multDigitsComp :: String -> Int
multDigitsComp ls = product [ digitToInt x | x <- ls, isDigit x]    -- obtinem lista cu caracterele transformate in cifre si functia product va realiza produsul elementelor din lista asta


