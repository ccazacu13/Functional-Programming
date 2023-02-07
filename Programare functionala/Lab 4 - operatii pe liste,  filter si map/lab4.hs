
--1. ---------------------------------------------------

-- Functie care intoarce lista divizorilor pozitivi ai lui n

factor :: Int -> [Int]
factor n = [x | x <- [1..n], n `mod` x == 0]

--2. ---------------------------------------------------

-- Functie care verifica daca un numar este prim

prim :: Int -> Bool
prim n = factor n == [1,n]  -- multimea divizorilor este 1 si nr

--3. ---------------------------------------------------

-- Functie care intoarce lista numerelor prime din [2..n], folosind functiile definite mai sus

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]   -- folosim functia prim sa verificam proprietatea

--4. ---------------------------------------------------

-- Functia myzip3 care se comporta ca functia zip, dar are 3 argumente, creeaza un tuplu de 3 elemente
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 l1 l2 l3 = [ (a,b,c) | (a,(b,c)) <- zip l1 $ zip l2 l3] -- cele doua zip uri ne dau elemente de forma (a, (b,c)) pe care le rescriem (a,b,c)

--5. ---------------------------------------------------

-- Functie care ea lista primelor elemente dintr-o lista de tupluri

firstEl :: [(a,b)] -> [a]
firstEl ls = map fst ls         -- fst scoate primul element dintr-un tuplu

-- firstEl = map fst // se poate scrie si asa, lista fiind trimisa implicit

--6. ---------------------------------------------------

-- Functie care ia o lista de liste de intregi si intoarce lista sumelor elementelor continute

sumList :: [[Int]] -> [Int]
sumList ls = map sum ls     -- functia sum calculeaza suma elementelor unei liste de intregi

--7. ---------------------------------------------------

-- Functie care intoarce o lista modificata astfel incat nr pare sunt injumatatite, iar cele impare dublate

prel2 :: [Int] -> [Int]
prel2 ls = map (\a -> if even a then a `div` 2 else a * 2) ls

--8. ---------------------------------------------------

-- Functie care primeste un caracter si o lista de siruri si returneaza lista de siruri care contine caracterul respectiv

siruri :: Char -> [String] -> [String]
siruri c ls = filter (elem c) ls    -- aplicam 'elem c' pe sirurile noastre si returneaza true daca caracterul se afla in el

--9. ---------------------------------------------------

-- Functie care primeste o lista de numere intregi si returneaza patratele numerelor impare

patrate :: [Int] -> [Int]
patrate ls = map (^2) $ filter odd ls   -- luam lista numerelor impare cu filter avand predicatul odd, si aplicam ridicarea la putere ^2 pt fiecare element

--10. ---------------------------------------------------

-- Functie care primeste o lista de numerer intregi si returneaza lista patratelor de pe pozitii impare

patratePozitii :: [Int] -> [Int]
patratePozitii ls = map ((^2) . snd) $ filter (odd . fst) $ zip [1..] ls    -- le atribuim elementelor din lista indecsi cu zip, apoi aplicam filter ca sa obtinem elementele cu indecsi impari 
                                                                            -- (odd . fst) i-a primul element din tuplul creat de zip (indecsul) si verifica daca e impar
                                                                            -- apoi aplicam ((^2) . snd) cu map pe fiecare element, care i-a nr de pe pozitia 2 din tuplu si il ridica la patrat, adica numerele originale
 
--11. ---------------------------------------------------

-- Functie care primeste o lista de siruri de caractere si returneaza lista sirurilor in care au fost eliminate consoanele

numaiVocale :: [String] -> [String]
numaiVocale ls = map (filter (`elem` "aeiouAEIOU")) ls  -- pt fiecare sir din lista vom filtra numai vocalele, predicatul din filter fiind `elem` "aeiouAEIOU"

--12. ---------------------------------------------------

-- Definiti recursiv mymap si myfilter cu aceleasi functionalitati ca cele predefinite

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x : xs) = f x : mymap f xs     -- aplicam functia f pe fiecare element al listei

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter pred (x : xs) = if pred x then x : myfilter pred xs   -- verificam daca predicatul returneaza true pentru numarul din lista, daca da il adaugam la rezultat
                            else myfilter pred xs

                            

