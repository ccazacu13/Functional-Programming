import Control.Applicative (Const)
import Data.ByteString.Builder.Extra (Next(More))
{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}

-- Scrieti instante ale clasei Functor pentru tipurile de date descrise mai jos

newtype Identity a = Identity a

instance Functor Identity  where
    fmap :: (a -> b) -> Identity a -> Identity b  
    fmap f (Identity a) = Identity (f a)

-----------------------------------

data Pair a = Pair a a

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair a b) = Pair (f a) (f b)

-----------------------------------

data Constant a b = Constant b 
                    deriving Show

instance Functor (Constant a) where
    fmap :: (b -> c) -> Constant a b -> Constant a c 
    fmap f (Constant b) = Constant $ f b

-----------------------------------

data Two a b = Two a b

instance Functor (Two a) where
    fmap :: (b -> c) -> Two a b -> Two a c
    fmap f (Two a b) = Two a (f b)

-----------------------------------

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three a b c) = Three a b (f c)

-----------------------------------

data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap :: (b -> c) -> Three' a b -> Three' a c
    fmap f (Three' a b c) = Three' a (f b) (f c)

-----------------------------------

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap :: (d -> e) -> Four a b c d -> Four a b c e
    fmap f (Four a b c d) = Four a b c (f d)

-----------------------------------

data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where
    fmap :: (b -> c) -> Four'' a b -> Four'' a c
    fmap f (Four'' a b c d) = Four'' a b c (f d)

-----------------------------------

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap :: (b -> c) -> Quant a b -> Quant a c
    fmap f (Bloor b) = Bloor (f b)
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a

-----------------------------------

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where           -- punem conditia de instanta de functor pentru f ca sa putem folosi fmap pentru tipul f
    fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
    fmap g (LiftItOut f) = LiftItOut $ fmap g f

-----------------------------------

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where      -- adaugam conditia de instanta de functor pentru f si g pentru a putea folosi fmap pe f si g
    fmap :: (a -> b) -> Parappa f g a -> Parappa f g b
    fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

-----------------------------------

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a)  where 
    fmap :: (b -> c) -> IgnoreOne f g a b -> IgnoreOne f g a c
    fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b) 
    
-----------------------------------

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where 
    fmap :: (t -> b) -> Notorious g o a t -> Notorious g o a b
    fmap f (Notorious a b c) = Notorious a b (fmap f c)

-----------------------------------

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap :: (a -> b) -> GoatLord a -> GoatLord b
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-----------------------------------

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap :: (a -> b) -> TalkToMe a -> TalkToMe b
    fmap _ Halt = Halt
    fmap f (Print x y) = Print x (f y)
    fmap f (Read g) = Read $ f . g      -- (a -> b) -> (String -> a) -> (String -> b)

