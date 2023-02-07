
--1. ---------------------------------------------------

-- Functie poly2 cu 4 argumente care calculeaza a * x ^ 2 + b * x + c

poly2 :: Num a => a -> a -> a -> a -> a
poly2 a b c x = a * x ^ 2 + b * x + c

--2. ---------------------------------------------------

-- Functie eeny care intoare 'eeny' pt numar par si 'meeny' pt numar impar

eeny :: Int -> String
eeny nr = if even nr 
            then "eeny"
            else "meeny"

--3. ---------------------------------------------------

-- Functie fizzbuzz care intoarce 'Fizz' pentru nr divizibile cu 3, 'Buzz' pt nr 
-- divizibile cu 5 si 'FizzBuzz' pt numerele divizibile cu ambele

fizzbuzz :: Int -> String
fizzbuzz nr 
        | mod nr 3 == 0 && mod nr 5 == 0 = "Fizzbuzz"
        | mod nr 3 == 0 = "Fizz"
        | mod nr 5 == 0 = "Buzz"
        | otherwise = "Numarul nu e divizivil cu numerele 3 sau 5"

--4. ---------------------------------------------------

-- Functie tribonacci facuta pe cazuri si ecuational
tribonacci_cazuri :: Integer -> Integer
tribonacci_cazuri 1 = 1
tribonacci_cazuri 2 = 1
tribonacci_cazuri 3 = 2
tribonacci_cazuri nr = tribonacci_cazuri (nr - 1) + tribonacci_cazuri (nr - 2) + tribonacci_cazuri(nr - 3)

tribonacci_ecuational :: Integer -> Integer
tribonacci_ecuational nr
                    | nr == 1 = 1
                    | nr == 2 = 1
                    | nr == 3 = 2
                    | otherwise = tribonacci_ecuational (nr - 1) + tribonacci_ecuational (nr - 2) + tribonacci_ecuational(nr - 3)

--5. ---------------------------------------------------

-- Functie care sa calculeze coeficientii binomiali folosind recursivitate

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k -1) 

--6. ---------------------------------------------------

-- Functie verifL care verifica daca lungimea unei liste e para

verifL :: [Int] -> Bool
verifL ls = even $ length ls -- aplicam even pe rezultatul functie length ls  

-- Functie takefinal care intoarce ultimele n elemente ale unei liste

takefinal :: [Int] -> Int -> [Int]
takefinal ls nr = drop (length ls - nr) ls  -- aflam cate elente vrem sa eliminam din lista si le scoatem folosind drop

-- Functie remove sterge elementul de pe pozitia n

remove :: [Int] -> Int -> [Int]
remove ls nr = take (nr - 1) ls ++ drop nr ls   -- luam primele nr - 1 elemente si le concatenam cu lista elementelor de la pozitia nr incolo

--7. ---------------------------------------------------

-- Functie myreplicate care primeste un nr intreg si o valoare v si intoarce o lista de forma [v ,v ,v ,v ,v] , nr = 5

myreplicate :: Integer -> Integer -> [Integer]
myreplicate 0 v = []
myreplicate nr v = v : myreplicate (nr - 1) v -- folosim contructorul de liste in mod recursiv pentru a adauga elementele la lista vida

-- Alternative

myreplicate1 :: Integer -> Integer -> [Integer]
myreplicate1 nr v 
                | nr == 0 = []
                | otherwise = v : myreplicate (nr - 1) v 

-- Functie sumImp care calculeaza suma valorilor impare dintr-o lista

sumImp :: [Integer] -> Integer
sumImp [] = 0
sumImp (x : xs) = if odd x                  -- daca capul listei e impar atunci o adunam la valoarea curenta
                    then x + sumImp xs
                    else sumImp xs          -- altfel o ignoram 

-- Functie totalLen care calculeaza suma lungimilor sirurilor care incep cu caracterul A

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (str : strs) = if take 1 str == "A" then length str + totalLen strs        -- luam prima litera cu take care va intoarce o lista cu primul caracter, ex:  [A]
                            else totalLen strs
 



