
data Fruct = Mar String Bool 
            | Portocala String Int

listaFructe = [Mar "Ionatan" False,
            Portocala "Sanguinello" 10,
            Portocala "Valencia" 22,
            Mar "Golden Delicious" True,
            Portocala "Sanguinello" 15,
            Portocala "Moro" 12,
            Portocala "Tarocco" 3,
            Portocala "Moro" 12,
            Portocala "Valencia" 2,
            Mar "Golden Delicious" False,
            Mar "Golden" False,
            Mar "Golden" True]

--1. ---------------------------------------------------

-- Functie care indica daca o portocala ete de Sicilia. Tipuri : Tarocco, Moro, Sanguinello

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala tip felii) = tip `elem` ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

-- Functie care calculeaza suma feliilor de portocale de Sicilia dintr-o lista de fructe

getFelii :: Fruct -> Int -- functie auxiliara care extrage numarul de felii
getFelii (Portocala str nr) = nr
getFelii _ = 0

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = foldr (\a b -> if ePortocalaDeSicilia a then b + getFelii a else b) 0  

-- Alternative

nrFeliiSicilia1 :: [Fruct] -> Int
nrFeliiSicilia1 ls = sum [nr  | Portocala tip nr <- ls, ePortocalaDeSicilia $ Portocala tip nr]  -- facem o lista care contine numarul de felii de portocale de sicilia, apoi adunam

-- Functie care calculeaza numarul de mere cu viermi dintr-o lista

nrMereViermi :: [Fruct] -> Int
nrMereViermi ls = length [1 | Mar tip True <- ls]   -- facem o lista care are numarul de elemente egal cu numarul de mere cu viermi, apoi returnam lungimea listei

--2. ---------------------------------------------------

type NumeA = String     -- type atribuie un alt nume tipului din dreapa, NumeA este acelasi lucru cu String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa 
                deriving Show

-- Functie vorbeste care intoarce "Meow!" pentru pisica si "Woof!" pentru caine


vorbeste :: Animal -> String    -- facem pattern matching
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

-- Functie care foloseste Maybe si intoarce rasa daca avem un caine sau Nothing daca este o pisica

rasa :: Animal -> Maybe Rasa
rasa (Caine _ r) = Just r
rasa _ = Nothing

--3. ---------------------------------------------------

data Linie = L [Int]
            deriving Show

data Matrice = M [Linie]
            deriving Show

-- Functie care verifica daca suma elementeor de pe fiecare linie este egala cu n

matrice1 = M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]
matrice2 = M [L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]

verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (\(L ls) b -> n == sum ls && b) True linii -- pentru fiecare linie din matrice luam lista de elemente 
                                                                        --pe care aplicam sum si comparam cu elementul neutru pe care il actualizam                                                                        

-- Functie care verifica daca toate liniile de lungime n ale matricei au elemente strict pozitive

doarPozN :: Matrice -> Int -> Bool
doarPozN (M linii) n = foldr (\(L ls) b-> if length ls == n then not (any (<0) ls) && b else b ) True linii -- trecem prin liste, daca are acelasi numar de elemente cu n atunci verificam daca exista
                                                                                                            -- un element negativ in lista cu functia any si modificam elementul neutru corespunzator, altfel lasam rezultatul nemodificat

-- Definiti predicatul corect care verifica daca toate liniile matricei au aceeasi lungime

corect :: Matrice -> Bool
corect (M []) = True    -- daca avem o lista vida returnam true
corect (M ((L l) : xs)) = foldr (\(L ls) b -> length ls == length l && b) True xs   --scoatem primul element din lista de linii so comparam lungimile celorlalte linii cu prima, updatam rezultatul corespunzator
                                                                        











