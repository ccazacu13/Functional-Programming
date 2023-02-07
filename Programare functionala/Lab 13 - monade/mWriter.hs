
--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } deriving Show 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)

--5.1.1 ---------------------------------------------------

-- Implementati functiile logIncrement si logIncrementN din curs

logIncrement :: Int  -> WriterS Int
logIncrement x = do
              tell ("increment: " ++ show x ++ "\n")
              return (x + 1)

-- Functie care ia un x si un numar de incrementari

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do
                      if n  == 1 
                        then logIncrement x
                      else do
                            val <- logIncrement x
                            logIncrementN val (n-1)

-- Alternative

-- varianta cu operatii monadice

logIncrementN1 :: Int -> Int -> WriterS Int
logIncrementN1 x 1 = logIncrement x
logIncrementN1 x n = logIncrement x >>= \val -> logIncrementN1 val (n-1) 

--5.2 ---------------------------------------------------   

-- Rescrieti definitia monadei WriterS a.i. sa produca lista mesajelor logate
                  

newtype WriterSLog a = WriterLog { runWriterLog :: (a, [(Int, String)]) } deriving Show 


instance  Monad WriterSLog where
  return va = WriterLog (va, [])
  ma >>= k = let (va, log1) = runWriterLog ma
                 (vb, log2) = runWriterLog (k va)
             in  WriterLog (vb, log1 ++ log2 )


instance  Applicative WriterSLog where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterSLog where              
  fmap f ma = pure f <*> ma     

tellLog :: (Int, String) -> WriterSLog () 
tellLog log = WriterLog ((), [log])

----------------------------
logIncrement2 :: Int  -> WriterSLog Int
logIncrement2 x = do
              tellLog (x, "increment: " ++ show x ++ "\n")
              return (x + 1)

logIncrementN12 :: Int -> Int -> WriterSLog Int
logIncrementN12 x 1 = logIncrement2 x
logIncrementN12 x n = logIncrement2 x >>= \val -> logIncrementN12 val (n-1) 