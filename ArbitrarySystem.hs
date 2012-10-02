{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleInstances #-}

module ArbitrarySystem where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen

import Singletons
import SecurityBasics

-- Arbitrary Nats, NatSets and Singletons

instance Arbitrary Nat where
    arbitrary = (arbitrary :: Gen (NonNegative Int)) >>= \(NonNegative x) -> return (intToNat x)

instance Arbitrary (Exists Singleton) where
    arbitrary = natToSingleton `fmap` (arbitrary :: Gen Nat)

instance (GenSingleton n) => Arbitrary (NatSet n) where
    arbitrary = elements $ allNS value

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
type ASObserIntermediate s d = Map (ASState s, ASDomain d) ObservationSymbol

type ASInterferences d = [(ASDomain d, ASDomain d)]

-- Arbitrary Security Policy

type ASPolicy d = (DomainList d, ASInterferences d)

data ExistsASPolicy where
    ExAP :: ASPolicy d -> ExistsASPolicy

-- Generates a list of NatSet n from a given singleton, then shrinks the list to a random size of [0, orig_size]

randomSubThing :: Singleton n -> Gen [ASThing n]
randomSubThing sing = do
    let orig_list = allNS sing                            -- generate all NatSet elements
    size <- choose (0, length orig_list)                  -- then pick a random size
    random_list <- (vectorOf size) $ (elements orig_list) -- generate random sublist
    return (List.nub random_list)                         -- remove duplicates and done!

-- Make all possible pairs of the elements in the two lists

allPairs :: [a] -> [b] -> [(a, b)]
allPairs as bs = concatMap (\a -> map (\b -> (a, b)) bs) as

-- Generator for Security Policy

instance Arbitrary (ExistsASPolicy) where
    arbitrary = do
        (ExistsNat (d_sing :: Singleton d)) <- (arbitrary :: Gen (Exists Singleton))
        let ds_ns = allNS d_sing
        inter_a <- randomSubThing d_sing
        inter_b <- randomSubThing d_sing
        let inter = allPairs inter_a inter_b
        return (ExAP (ds_ns, inter))

instance Show ExistsASPolicy where
    show (ExAP (ds_ns, inter)) = "QC\n" ++
                                 "Domains: " ++ (show ds_ns) ++ "\n" ++
                                 "Interferences: " ++ (show inter)

-- Generator for Intermediate System

type ASIntermediate s a d =
    (StateList s, DomainList d, ActionList a, ASDomIntermediate d a,
     ASTransIntermediate s a, ASObserIntermediate s d, ASInterferences d, ASState s)

data ExistsASIntermediate where
    ExAI :: ASIntermediate s a d -> ExistsASIntermediate

instance Arbitrary (ExistsASIntermediate) where
    arbitrary = do
        (ExAP (ds_ns, inter))               <- (arbitrary :: Gen (ExistsASPolicy))
        (ExistsNat (s_sing :: Singleton s)) <- (arbitrary :: Gen (Exists Singleton))
        (ExistsNat (a_sing :: Singleton a)) <- (arbitrary :: Gen (Exists Singleton))
        let ss_ns = allNS s_sing
        let as_ns = allNS a_sing
        init  <- elements ss_ns
        dom   <- (\a -> do d <- elements ds_ns; return (d, a)) `mapM` as_ns
        trans <- Map.fromList `fmap` ((\key -> do to <- elements ss_ns; return (key, to)) `mapM` (allPairs ss_ns as_ns))
        let sd_pairs = allPairs ss_ns ds_ns
        obser <- Map.fromList `fmap` ((\key -> do obs <- choose (1, length sd_pairs); return (key, show obs)) `mapM` sd_pairs)
        return (ExAI (ss_ns, ds_ns, as_ns, dom, trans, obser, inter, init))

-- System package

asPolicyInter :: ASInterferences d -> ASDomain d -> ASDomain d -> Bool
asPolicyInter inter_ns a b = List.elem (a, b) inter_ns

asBuildPolicy :: ASInterferences d -> Policy (ASDomain d)
asBuildPolicy inter_ns = Policy { inter = asPolicyInter inter_ns }

as_step :: ASTransIntermediate s a -> ASState s -> ASAction a -> ASState s
as_step trans from action = trans Map.! (from, action)

as_obs :: ASObserIntermediate s d -> ASState s -> ASDomain d -> ObservationSymbol
as_obs obs s d = obs Map.! (s, d)

as_dom :: ASDomIntermediate d a -> ASAction a -> ASDomain d
as_dom dom action = case List.find (\(d, a) -> a == action) dom of
    Just (d, a) -> d
    Nothing     -> error ("AS Dom: domain for action " ++ show action ++ " was not found.")

buildAS :: ASIntermediate s a d -> System (ASState s) (ASAction a) ObservationSymbol (ASDomain d)
buildAS (ss, ds, as, dom, trans, obser, inter, init) = System {
    initial     = init,
    step        = as_step trans,
    obs         = as_obs obser,
    dom         = as_dom dom,
    policy      = asBuildPolicy inter
}

data ASSystem s a d = ASSystem {
    base :: System (ASState s) (ASAction a) ObservationSymbol (ASDomain d),
    intermediate :: ASIntermediate s a d
}

data ExistsASSystem where
    ExAS :: ASSystem s d a -> ExistsASSystem

buildASExists :: ExistsASIntermediate -> ExistsASSystem
buildASExists (ExAI intermediate) = ExAS (ASSystem (buildAS intermediate) intermediate)

instance Show ExistsASSystem where
    show (ExAS sys) = "=== QuickCheck generated system ===\n" ++
                      "Initial state: " ++ show (initial base_sys) ++ "\n" ++
                      "States: " ++ show ss_ns ++ "\n" ++
                      "Actions: " ++ show dom ++ "\n" ++
                      "Transitions: " ++ show_trans ++ "\n" ++
                      "Observatons: " ++ show_obs
        where base_sys = base sys
              (ss_ns, ds_ns, as_ns, dom, trans_ns, obser, _, _) = intermediate sys
              show_trans = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\((from, action), to) -> "(" ++ show from ++ ", " ++ show action ++ ") ~> " ++ show to) $
                            Map.toList trans_ns)
              show_obs   = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\((s, d), o) -> "(" ++ show s ++ ", " ++ show d ++ ") >? " ++ show o) $
                            Map.toList obser)

instance Arbitrary (ExistsASSystem) where
    arbitrary = do
        intermediate <- (arbitrary :: Gen ExistsASIntermediate)
        return (buildASExists intermediate)

