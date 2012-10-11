{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances #-}

module QuickSCS.TypeNats where

-- Peano naturals, used to represent Ints on type level
data Nat = Zero | Suc Nat deriving (Eq, Ord)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n - 1))

natToInt :: Nat -> Int
natToInt Zero    = 0
natToInt (Suc n) = 1 + natToInt n

instance Show Nat where
    show x = "n" ++ show (natToInt x)

-- Finite set with a Peano natural to specify number of elements
data NatSet :: Nat -> * where
    NSZero :: NatSet (Suc a)
    NSSuc  :: NatSet a -> NatSet (Suc a)

instance Show (NatSet n) where
    show x = "NS" ++ show (natToInt $ nsToNat x)

instance Eq (NatSet n) where
--    (==) m n = (nsToNat m) == (nsToNat n)
    (==) NSZero    NSZero    = True
    (==) NSZero    _         = False
    (==) _         NSZero    = False
    (==) (NSSuc m) (NSSuc n) = m == n

instance Ord (NatSet n) where
    compare m n = compare (nsToNat m) (nsToNat n)

nsToNat :: NatSet n -> Nat
nsToNat NSZero    = Zero
nsToNat (NSSuc n) = Suc (nsToNat n)

-- Singleton type
data Singleton :: Nat -> * where
    SZero :: Singleton Zero
    SSuc  :: Singleton n -> Singleton (Suc n)

-- Given a Nat singleton, generate all elements in the NatSet of same number
allNS :: Singleton n -> [NatSet n]
allNS (SZero)  = []
allNS (SSuc n) = NSZero : map NSSuc (allNS n)

-- "Reify" type class that allows type-level Nat to be converted to Singleton
class GenSingleton (n :: Nat) where
    value :: Singleton n

instance GenSingleton Zero where
    value = SZero

instance GenSingleton n => GenSingleton (Suc n) where
    value = SSuc (value)

-- Reverse-conversion from Singleton to Nat
singletonToNat :: Singleton n -> Nat
singletonToNat SZero    = Zero
singletonToNat (SSuc n) = Suc $ singletonToNat n

-- Wrapper type that allows stuff like NatSet and Singleton to be generated at run-time
data Exists :: (Nat -> *) -> * where
    ExistsNat :: (GenSingleton x) => v x -> Exists v
    ExistsOnly :: v x -> Exists v

-- This is not ideal. There should be better way to do this.
instance Eq (Exists Singleton) where
    (==) (ExistsNat m) (ExistsNat n) = (singletonToNat m) == (singletonToNat n)

-- Oh god.
instance Ord (Exists Singleton) where
    compare (ExistsNat m) (ExistsNat n) = compare (singletonToNat m) (singletonToNat n)

-- Then we can have this
natToSingleton :: Nat -> Exists Singleton
natToSingleton n = natToSingletonRec (ExistsNat SZero) n
    -- Recursive function to build up Singleton from given Nat
    where natToSingletonRec :: Exists Singleton -> Nat -> Exists Singleton
          natToSingletonRec s Zero = s
          natToSingletonRec (ExistsNat m) (Suc n) = natToSingletonRec (ExistsNat (SSuc m)) n

-- or even better, this
intToSingleton :: Int -> Exists Singleton
intToSingleton n = natToSingleton $ intToNat n

natToNatSet :: Nat -> Exists NatSet
natToNatSet n = natToNatSetRec (ExistsOnly NSZero) n
    -- Recursive function to build up NatSet from given Nat
    where natToNatSetRec :: Exists NatSet -> Nat -> Exists NatSet
          natToNatSetRec s Zero = s
          natToNatSetRec (ExistsOnly m) (Suc n) = natToNatSetRec (ExistsOnly (NSSuc m)) n

instance Show (Singleton n) where
    show n = "Singleton " ++ show (natToInt $ singletonToNat n)

instance Show (Exists Singleton) where
    show (ExistsNat n) = "ExistsNat (" ++ show n ++ ")"

instance Show (Exists NatSet) where
    show (ExistsOnly n) = "ExistsOnly (" ++ show n ++ ")"

