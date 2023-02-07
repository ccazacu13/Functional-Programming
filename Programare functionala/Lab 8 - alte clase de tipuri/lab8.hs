data Punct = Pt [Int]

--1.a ---------------------------------------------------

-- Sa se scrie o instanta a clasei Show pentru tipul de date Punct ai coordonatele sa fie afisate ca tuplu

instance Show Punct where
  show :: Punct -> String
  show (Pt []) = "()"
  show (Pt ls) = "(" ++ afisare ls ++ ")"
                where 
                   afisare [x] = show x -- folosim o functie auxiliara pentru a nu avea prea multe paranteze
                   afisare (x : xs) = show x ++ ", " ++  afisare xs  

--1.b ---------------------------------------------------

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
      toArb :: a -> Arb
      fromArb :: Arb -> a

-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)

-- Sa se scrie o instanta a clasei ToFromArb pentru tipul de date Punct

instance ToFromArb Punct where
  toArb :: Punct -> Arb
  toArb (Pt []) = Vid
  toArb(Pt (x : xs)) = N (F x) $ toArb (Pt xs)

  fromArb :: Arb -> Punct
  fromArb Vid = Pt []
  fromArb (F a) = Pt [a]
  fromArb (N a1 a2) = Pt $ l1 ++ l2
                    where Pt l1 = fromArb a1
                          Pt l2 = fromArb a2 

--2 ---------------------------------------------------

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

--2.a ---------------------------------------------------

--Sa se instantieze clasa GeoOps pentru tipul de date Get

instance GeoOps Geo where
  perimeter :: (Floating a) => Geo a -> a
  perimeter (Square a) = 4 *a
  perimeter (Rectangle a b) = 2 * a + 2 * b
  perimeter (Circle r) = 2 * pi * r

  area :: (Floating a) => Geo a -> a
  area (Square a) = a * a
  area (Rectangle a b) = a * b
  area (Circle r) = pi * r * r

-- ghci> pi
-- 3.141592653589793


--2.b ---------------------------------------------------

-- Sa se intantieze clasa Eq pentru tipul de date Geo, ai doua figuri geometrice sunt egale
-- daca au perimetrul egal

instance (Eq a, Floating a) => Eq (Geo a) where -- garantam ca tipurile de date vor avea instante pentru Eq si Floating pentru
  (==) :: Geo a -> Geo a -> Bool                -- a putea folosi functia perimeter si operatorul ==
  g1 == g2 = perimeter g1 == perimeter g2
