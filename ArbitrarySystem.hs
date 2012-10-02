{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleInstances #-}

module ArbitrarySystem where

import qualified Data.Map as Map
import Data.Map (Map)
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

type ASDomIntermediate d a = [(ASDomain d, ASAction a)]
type ASTransIntermediate s a = Map (ASState s, ASAction a) (ASState s)

{-
type ObserIntermediate s d = Map (StateAssoc s, DomainAssoc d) ObservationSymbol
type InterferenceAssoc n = [(DomainAssoc n, DomainAssoc n)]
type ArbitrarySystemIntermediate s a d =
    (StateAList s, DomainAList d, ActionAList a, DomIntermediate d a,
     TransIntermediate s a, ObserIntermediate s d, InterferenceAssoc d, StateAssoc s)
-}
type ASIntermediate s a d =
    (StateList s, DomainList d, ActionList a, ASDomIntermediate d a, ASTransIntermediate s a, ASState s)

data ExistsASIntermediate where
    ExAI :: ASIntermediate s d a -> ExistsASIntermediate

instance Arbitrary Nat where
    arbitrary = (arbitrary :: Gen (NonNegative Int)) >>= \(NonNegative x) -> return (intToNat x)

instance Arbitrary (Exists Singleton) where
    arbitrary = natToSingleton `fmap` (arbitrary :: Gen Nat)

instance (GenSingleton n) => Arbitrary (NatSet n) where
    arbitrary = elements $ allNS value

allPairs :: [a] -> [b] -> [(a, b)]
allPairs as bs = concatMap (\a -> map (\b -> (a, b)) bs) as

instance Arbitrary (ExistsASIntermediate) where
    arbitrary = do
        (ExistsNat (s_sing :: Singleton s)) <- (arbitrary :: Gen (Exists Singleton))
        (ExistsNat (d_sing :: Singleton d)) <- (arbitrary :: Gen (Exists Singleton))
        (ExistsNat (a_sing :: Singleton a)) <- (arbitrary :: Gen (Exists Singleton))
        let ss_ns = allNS s_sing
        let ds_ns = allNS d_sing
        let as_ns = allNS a_sing
        init  <- elements ss_ns
        dom   <- (\a -> do d <- elements ds_ns; return (d, a)) `mapM` as_ns
        trans <- Map.fromList `fmap` ((\key -> do to <- elements ss_ns; return (key, to)) `mapM` (allPairs ss_ns as_ns))
        return (ExAI (ss_ns, ds_ns, as_ns, dom, trans, init))

instance Show ExistsASIntermediate where
    show (ExAI (ss_ns, ds_ns, as_ns, dom, trans, init)) = "QC\n" ++
                                              "States: " ++ (show ss_ns) ++ "\n" ++
                                              "Domains: " ++ (show ds_ns) ++ "\n" ++
                                              "Actions: " ++ (show as_ns) ++ "\n" ++
                                              "Dom: " ++ (show dom) ++ "\n" ++
                                              "Transitions: " ++ (show trans) ++ "\n" ++
                                              "Initial: " ++ (show init)

