
{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

--1.2 ---------------------------------------------------

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

-- Definiti functia fct folosind notatia do

fctDo mx = do
          nr <- mx          -- scoatem numarul din maybe
          return (pos nr)   -- obtinem (pos nr) si il introducem din nou in maybe cu return

-- Alternative

fctDo1 :: Monad f => f Int -> f Bool
fctDo1 mx = pos <$> mx    --putem folosi fmap (<$>) ca sa scoatem intregul din mx si apoi sa-l modificam

--2. ---------------------------------------------------

-- Functie care aduna doua valori de tip Maybe

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = do 
            x <- mx             -- scoatem int urile din maybe
            y <- my
            return (x + y)      -- adunam valorile si le reintoarcem in maybe

-- Alternative

-- scriere cu secventiere (cu operatii monadice >>=)

--(>>=) :: Monad m => m a -> (a -> m b) -> m b
addM1 :: Maybe Int -> Maybe Int -> Maybe Int            
addM1 mx my = mx >>= \x -> my >>= \y -> return (x + y)  -- folosim operatiile monadice pt a scoate x si y din monadele lor si la final returnam suma lor incapsulata in monada

-- scriere folosind operatiile din applicative si functor

addM2 :: Maybe Int -> Maybe Int -> Maybe Int
addM2 mx my = fmap (+) mx <*> my                      -- fmap scoate x-ul din mx, iar apoi obtinem argumentul doi necesar sumei folosind <*> operatorul din applicative 

-- scriere folosind sabloane

addM3 :: Maybe Int -> Maybe Int -> Maybe Int          -- verificam cazurile
addM3 Nothing _ = Nothing
addM3 _ Nothing = Nothing
addM3 (Just x) (Just y) = Just $ x + y

--3. ---------------------------------------------------

-- Sa se treaca in notatia do urmatoarele functii:

--Functie care calculeaza produsul cartezian
cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_productDo :: Monad m => m a -> m b -> m (a, b)
cartesian_productDo xs ys = do
                          x <- xs         -- scoatem x si y
                          y <- ys
                          return (x, y)   -- returnam tuplurile


-- Functie care care construieste lista rezultatelor functie f aplicata pe toate combinatiile posibile x din xs si y din ys
prod f xs ys = [f x y | x <- xs,  y<-ys]

prod1 :: (a -> b -> c) -> [a] -> [b] -> [c]
prod1 f xs ys = do
                x <- xs
                y <- ys
                return (f x y)

-- Functie care citeste un sir de caractere de la tastatura pana la \n

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine1 :: IO String
myGetLine1 = do
            x <- getChar        --obtinem un caracter de la tastatura
            if x == '\n' then   --daca este new line returnam lista vida
              return []     
            else do
                xs <- myGetLine1 -- altfel mergem recursiv sa obtinem toate celelalte caractere concatenate
                return (x : xs)  -- reconstruim lista

--4. ---------------------------------------------------

-- Sa se treaca in notatia cu secventiere urmatoarele functii (>>=)

prelNo noin =  sqrt noin

-- Functie care afiseaza radicalul unui numar dat de la tastatura
ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber1 :: IO ()
ioNumber1 = (readLn :: IO Float) >>= \noin -> putStrLn ("Intrare\n" ++ show noin) >> putStrLn "Iesire" >> print (prelNo noin) -- luam numarul de la tastatura, facem afisarile si apoi afisam rezultatul radicalului (prel noin)
                                                                                                                              -- important la secventiere este ca ultima valoare sa fie intoarsa in monada folosind return sau alta functie

--5. --------------------------------------------------- SE AFLA IN FISIERUL WriterS

--6.1 ---------------------------------------------------

-- Functiile showPersonN si showPersnoA care afiseaza frumos numele si varsta

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN pers = "NAME: " ++ name pers
showPersonA :: Person -> String
showPersonA pers = "AGE: " ++ show (age pers) 

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

--6.2 ---------------------------------------------------

-- Functie care afiseaza frumos toate datele unei persoane, folosind functiile de mai sus

showPerson :: Person -> String
showPerson pers = "(" ++ showPersonN pers ++ ", " ++ showPersonA pers ++ ")"

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}

--6.3 ---------------------------------------------------

-- Folosind monada Reader definiti variante monadice pentru cele 3 functii de mai sus

newtype Reader env a = Reader { runReader :: env -> a }

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

----------------------------------

ask = Reader id   --functia cu care obtinem environment-ul

mshowPersonN ::  Reader Person String
mshowPersonN = ask >>= \env -> return $ "NAME: " ++ name env

-- Alternativa cu do

mShowPersonNDo :: Reader Person String
mShowPersonNDo = do
                env <- ask                    --obtinem persoana cu functia ask
                return ("NAME: " ++ name env) --returnam string ul

-- runReader mshowPersonN  $ Person "ada" 20
-- "NAME:ada"

----------------------------------

mshowPersonA ::  Reader Person String
mshowPersonA = ask >>= \env -> return $ "AGE: " ++ show (age env)

-- Alternativa cu do

mshowPersonADo = do
                env <- ask
                return ("AGE: " ++ show (age env))


-- runReader mshowPersonA  $ Person "ada" 20
-- "AGE:20"

----------------------------------

mshowPerson ::  Reader Person String
mshowPerson = mshowPersonN >>= \name -> mshowPersonA >>= \age -> return $ "(" ++ name ++ ", " ++ age ++ ")"   

mshowPersonDo :: Reader Person String
mshowPersonDo = do
              name <- mshowPersonN
              age <- mshowPersonA
              return $ "(" ++ name ++ ", " ++ age ++ ")"   
              

-- runReader mshowPerson  $ Person "ada" 20
-- "(NAME:ada,AGE:20)"
