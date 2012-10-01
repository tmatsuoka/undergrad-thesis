{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleInstances #-}

module ArbitrarySystem where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen

import Singletons
import SecurityBasics

data ExistsSystem where
    ExS :: System s a o d -> ExistsSystem

type ASThing  n = NatSet n
type ASState  n = ASThing n
type ASDomain n = ASThing n
type ASAction n = ASThing n

type ASList     n = [ASThing n]
type StateList  n = [ASState n]
type DomainList n = [ASDomain n]
type ActionList n = [ASAction n]

{-
type DomIntermediate d a = [(DomainAssoc d, ActionAssoc a)]
type TransIntermediate s a = Map (StateAssoc s, ActionAssoc a) (StateAssoc s)
type ObserIntermediate s d = Map (StateAssoc s, DomainAssoc d) ObservationSymbol
type InterferenceAssoc n = [(DomainAssoc n, DomainAssoc n)]
type ArbitrarySystemIntermediate s a d =
    (StateAList s, DomainAList d, ActionAList a, DomIntermediate d a,
     TransIntermediate s a, ObserIntermediate s d, InterferenceAssoc d, StateAssoc s)
-}
type ASIntermediate s a d =
    (StateList s, DomainList d, ActionList a, ASState s)

data ExistsASIntermediate where
    ExAI :: ASIntermediate s d a -> ExistsASIntermediate

instance Arbitrary Nat where
    arbitrary = (arbitrary :: Gen (NonNegative Int)) >>= \(NonNegative x) -> return (intToNat x)

instance Arbitrary (Exists Singleton) where
    arbitrary = natToSingleton `fmap` (arbitrary :: Gen Nat)

instance (GenSingleton n) => Arbitrary (NatSet n) where
    arbitrary = elements $ allNS value

instance Arbitrary (ExistsASIntermediate) where
    arbitrary = do
        s_exsing <- (arbitrary :: Gen (Exists Singleton))
        d_exsing <- (arbitrary :: Gen (Exists Singleton))
        a_exsing <- (arbitrary :: Gen (Exists Singleton))
        case s_exsing of
          (ExistsNat (s_sing :: Singleton s)) -> case d_exsing of
            (ExistsNat (d_sing :: Singleton d)) -> case a_exsing of
              (ExistsNat (a_sing :: Singleton a)) -> do
                init <- elements $ ss_ns
                return (ExAI (ss_ns, ds_ns, as_ns, init))
                  where ss_ns = allNS s_sing
                        ds_ns = allNS d_sing
                        as_ns = allNS a_sing

instance Show ExistsASIntermediate where
    show (ExAI (ss_ns, ds_ns, as_ns, init)) = "QC\n" ++
                                              "States: " ++ (show ss_ns) ++ "\n" ++
                                              "Domains: " ++ (show ds_ns) ++ "\n" ++
                                              "Actions: " ++ (show as_ns) ++ "\n" ++
                                              "Initial: " ++ (show init)

