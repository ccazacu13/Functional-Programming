
--1. ---------------------------------------------------

-- Functie cu 2 parametrii care calculeaza suma patratelor lor

patrat :: Num a => a -> a -> a
patrat x y = x^2 + y^2


--2. ---------------------------------------------------

-- Functie cu 1 paramteru care intoarce 'par' daca parametrul e par sau 'impar' altfel

par :: Int -> String
par x = if mod x 2 == 0     -- functie care calculeaza modulo
        then
            "par"
        else
            "impar"

-- Alternative

par1 x = if even x then "par"
            else "impar"

--3. ---------------------------------------------------

-- Functie care calculeaza factorialul unui numar

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

--4. ---------------------------------------------------

-- Functie care verifica daca primul parametru este mai mare decat dublul celui de-al doilea

verifica :: (Ord a, Num a) => a -> a -> Bool
verifica x y = x > y * 2
