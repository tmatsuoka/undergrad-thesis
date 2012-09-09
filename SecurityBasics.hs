{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances, ScopedTypeVariables #-}

module SecurityBasics where

import Data.List

-- Peano naturals, used to represent Ints on type level
data Nat = Zero | Suc Nat

toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

toInt :: Nat -> Int
toInt Zero    = 0
toInt (Suc n) = 1 + toInt n

instance Show Nat where
    show x = "n" ++ show (toInt x)

-- Finite set with a Peano natural to specify number of elements
data NatSet :: Nat -> * where
    NSZero :: NatSet a
    NSSuc  :: NatSet a -> NatSet (Suc a)

instance Show (NatSet n) where
    show x = "NS" ++ show (toInt (sizeNS x))

sizeNS :: NatSet n -> Nat
sizeNS NSZero    = Zero
sizeNS (NSSuc n) = Suc (sizeNS n)

-- Singleton type
data Singleton :: Nat -> * where
    SZero :: Singleton Zero
    SSuc  :: Singleton n -> Singleton (Suc n)

allNS :: Singleton n -> [NatSet n]
allNS (SZero)  = [NSZero]
allNS (SSuc n) = NSZero : map NSSuc (allNS n)

permuteNS :: Singleton n -> [[NatSet n]]
permuteNS s = concatMap permutations $ subsequences $ allNS s

data Policy a = Policy { inter :: a -> a -> Bool }
noninter (Policy f) a b = not (f a b)

-- System is also (semi-)parametrised by the domains (and thus policies) too.
data System state action obs domain = System
                { initial :: state,
                  step :: state -> action -> state,
                  obs :: state -> domain -> obs,
                  dom :: action -> domain,
                  action_list :: [action] --temporary
                };


{-
instance (Arbitrary s, Arbitrary a, Arbitary d) => Arbitrary (System s a d)


data ExistsSystem :: * where
   ExI :: System s a d -> ExistsSystem
-}

