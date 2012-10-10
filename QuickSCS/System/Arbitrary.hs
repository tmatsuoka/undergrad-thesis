{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleInstances #-}

module ICS.System.Arbitrary where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen

import ICS.TypeNats
import ICS.Utility
import ICS.System.Basics

-- Arbitrary Nats, NatSets and Singletons

instance Arbitrary Nat where
    arbitrary      = (arbitrary :: Gen (Positive Int)) >>= \(Positive x) -> return (intToNat x)
    shrink (Suc n) = [n]
    shrink Zero    = []

instance Arbitrary (Exists Singleton) where
    arbitrary = natToSingleton `fmap` (arbitrary :: Gen Nat)

instance (GenSingleton n) => Arbitrary (NatSet n) where
    arbitrary = elements $ allNS value

type ASThing  n = NatSet n
type ASState  n = ASThing n
type ASDomain n = ASThing n
type ASAction n = ASThing n

type ASList     n = [ASThing n]
type StateList  n = [ASState n]
type DomainList n = [ASDomain n]
type ActionList n = [ASAction n]

type ASDomIntermediate d a = [(ASDomain d, ASAction a)]
type ASTransIntermediate s a = Map (ASState s, ASAction a) [(ASState s)]
type ASObserIntermediate s d = Map (ASState s, ASDomain d) ObservationSymbol

type ASInterferences d = [(ASDomain d, ASDomain d)]

-- Arbitrary Security Policy

type ASPolicy d = (DomainList d, ASInterferences d)

data ExistsASPolicy where
    ExAP :: (GenSingleton d) => ASPolicy d -> ExistsASPolicy

-- Generates a list of NatSet n from a given singleton, then shrinks the list to a random size of [0, orig_size]

randomSubThing :: Singleton n -> Gen [ASThing n]
randomSubThing sing = do
    let orig_list = allNS sing                            -- generate all NatSet elements
    if (List.null orig_list) then
        return []
    else do
        size <- choose (1, length orig_list)                  -- then pick a random size
        random_list <- (vectorOf size) $ (elements orig_list) -- generate random sublist
        return (List.nub random_list)                         -- remove duplicates and done!

-- Generator for Security Policy

instance Arbitrary (ExistsASPolicy) where
    arbitrary = do
        (ExistsNat (d_sing :: Singleton d)) <- (arbitrary :: Gen (Exists Singleton))
        let ds_ns = allNS d_sing
        let inter_reflex = map (\d -> (d, d)) ds_ns
        inter_a <- randomSubThing d_sing
        inter_b <- randomSubThing d_sing
        let inter = List.nub $ inter_reflex ++ (allPairs inter_a inter_b)
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
    ExAI :: (GenSingleton s, GenSingleton a, GenSingleton d) => ASIntermediate s a d -> ExistsASIntermediate

genASIntermediate' :: (GenSingleton d) => DomainList d -> ASInterferences d -> Gen ExistsASIntermediate
genASIntermediate' ds_ns inter = do
    (ExistsNat (s_sing :: Singleton s)) <- (arbitrary :: Gen (Exists Singleton))
    (ExistsNat (a_sing :: Singleton a)) <- (arbitrary :: Gen (Exists Singleton))
    let ss_ns = allNS s_sing
    let as_ns = allNS a_sing
    init  <- elements ss_ns
    dom   <- (\a -> do d <- elements ds_ns; return (d, a)) `mapM` as_ns
    -- First line: generates input enabled non-deterministic systems.
    -- trans <- Map.fromList `fmap` ((\key -> do to <- randomSubThing s_sing; return (key, to)) `mapM` (allPairs ss_ns as_ns))

    -- Second line: generates deterministic systems.
    trans <- Map.fromList `fmap` ((\key -> do to <- elements ss_ns; return (key, [to])) `mapM` (allPairs ss_ns as_ns))
    let sd_pairs = allPairs ss_ns ds_ns
    obser <- Map.fromList `fmap` ((\key -> do obs <- choose (1, length sd_pairs); return (key, show obs)) `mapM` sd_pairs)
    return (ExAI (ss_ns, ds_ns, as_ns, dom, trans, obser, inter, init))

genASIntermediate :: Maybe ExistsASPolicy -> Gen ExistsASIntermediate
genASIntermediate (Just (ExAP (ds_ns, inter))) = genASIntermediate' ds_ns inter
genASIntermediate Nothing                      = do
    (ExAP (ds_ns, inter)) <- (arbitrary :: Gen (ExistsASPolicy))
    genASIntermediate' ds_ns inter

instance Arbitrary ExistsASIntermediate where
    arbitrary = genASIntermediate Nothing

-- System package

asPolicyInter :: ASInterferences d -> ASDomain d -> ASDomain d -> Bool
asPolicyInter inter_ns a b = List.elem (a, b) inter_ns

asBuildPolicy :: ASInterferences d -> Policy d
asBuildPolicy inter_ns = Policy { inter = asPolicyInter inter_ns }

as_step :: ASTransIntermediate s a -> ASState s -> ASAction a -> [ASState s]
as_step trans from action = trans Map.! (from, action)

as_obs :: ASObserIntermediate s d -> ASState s -> ASDomain d -> ObservationSymbol
as_obs obs s d = obs Map.! (s, d)

as_dom :: ASDomIntermediate d a -> ASAction a -> ASDomain d
as_dom dom action = case List.find (\(d, a) -> a == action) dom of
    Just (d, a) -> d
    Nothing     -> error ("AS Dom: domain for action " ++ show action ++ " was not found.")

buildAS :: ASIntermediate s a d -> System s a d
buildAS (ss, ds, as, dom, trans, obser, inter, init) = System {
    initial     = init,
    step        = as_step trans,
    obs         = as_obs obser,
    dom         = as_dom dom,
    policy      = asBuildPolicy inter
}

data ASSystem s a d where
    AS :: (GenSingleton s, GenSingleton a, GenSingleton d) => System s a d -> ASIntermediate s a d -> ASSystem s a d

data ExistsASSystem where
    ExAS :: (GenSingleton s, GenSingleton a, GenSingleton d) => ASSystem s a d -> ExistsASSystem

getASBase :: ASSystem s a d -> System s a d
getASBase (AS base _) = base

getASIntermediate :: ASSystem s a d -> ASIntermediate s a d
getASIntermediate (AS _ intermediate) = intermediate

buildASExists :: ExistsASIntermediate -> ExistsASSystem
buildASExists (ExAI intermediate) = ExAS (AS (buildAS intermediate) intermediate)

showNSPrefix :: String -> NatSet n -> String
showNSPrefix prefix val = prefix ++ show (natToInt $ nsToNat val)

instance Show ExistsASSystem where
    show (ExAS sys) = "=== QuickCheck generated system ===\n" ++
                      "arbitrary_policy = do\n" ++
                      "    domains " ++ show ds_string ++ "\n" ++
                      show_inter ++ "\n" ++
                      "\n" ++
                      "arbitrary_system = do\n" ++
                      "    start \"" ++ showNSPrefix "s" (initial base_sys) ++ "\"\n" ++
                      show_as ++ "\n" ++
                      show_trans ++ "\n" ++
                      show_obs
        where base_sys = getASBase sys
              (ss_ns, ds_ns, as_ns, dom, trans_ns, obser, inter_ns, _) = getASIntermediate sys
              ss_string  = map (showNSPrefix "s") ss_ns
              ds_string  = map (showNSPrefix "d") ds_ns
              as_reassembled = map (\d -> (d, map (\(_, a) -> showNSPrefix "a" a) $ List.filter (\(domain, _) -> d == domain) dom)) ds_ns
              as_trimmed = List.filter (\(d, as) -> not $ List.null as) as_reassembled
              show_as    = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\(d, as) -> "    actions \"" ++ showNSPrefix "d" d ++ "\" " ++ show as) as_trimmed)
              show_trans = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\((from, action), to) -> "    (\"" ++ showNSPrefix "s" from ++ "\", \"" ++ showNSPrefix "a" action ++ "\") ~> " ++
                                 (show $ map (showNSPrefix "s") to)) $
                            Map.toList trans_ns)
              show_obs   = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\((s, d), o) -> "    (\"" ++ showNSPrefix "s" s ++ "\", \"" ++ showNSPrefix "d" d ++ "\") >? " ++ show o) $
                            Map.toList obser)
              show_inter = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\(d1, d2) -> "    \"" ++ showNSPrefix "d" d1 ++ "\" >-> \"" ++ showNSPrefix "d" d2 ++ "\"") inter_ns)

genASSystem :: Maybe ExistsASPolicy -> Gen ExistsASSystem
genASSystem maybe_policy = do
    intermediate <- genASIntermediate maybe_policy
    return (buildASExists intermediate)

instance Arbitrary ExistsASSystem where
    arbitrary = genASSystem Nothing

