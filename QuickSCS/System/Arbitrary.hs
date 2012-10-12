{-# LANGUAGE DoAndIfThenElse, GADTs, ScopedTypeVariables, FlexibleInstances #-}

module QuickSCS.System.Arbitrary where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen

import QuickSCS.TypeNats
import QuickSCS.Utility
import QuickSCS.System.Basics

-- Arbitrary Nats, NatSets and Singletons

instance Arbitrary Nat where
    arbitrary = (arbitrary :: Gen (Positive Int)) >>= \(Positive x) -> return (intToNat x)
    -- shrink n  = map intToNat (shrink $ natToInt n)

instance Arbitrary (Exists Singleton) where
    arbitrary            = natToSingleton `fmap` (arbitrary :: Gen Nat)
    -- shrink (ExistsNat n) = natToSingleton `fmap` (shrink $ singletonToNat n)

genMinBoundedSingleton :: Int -> Gen (Exists Singleton)
genMinBoundedSingleton minimum = sized (\size -> do val <- choose (minimum, minimum `max` size)
                                                    return (natToSingleton $ intToNat val))

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

randomPick' :: [a] -> Int -> Gen [a]
randomPick' [] _                            = return []
randomPick' xs new_length | new_length == 0 = return []
                          | otherwise       = do new_index  <- choose (0, (length xs) - 1)
                                                 let new_pick  = xs List.!! new_index
                                                 let remainder = take new_index xs ++ drop (new_index + 1) xs
                                                 remainder_xs <- randomPick' remainder (new_length - 1)
                                                 return (new_pick : remainder_xs)

randomPick :: [a] -> Gen [a]
randomPick xs = do new_length <- choose (1, length xs)
                   randomPick' xs new_length

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
    -- (ExistsNat (s_sing :: Singleton s)) <- genMinBoundedSingleton (List.length ds_ns)
    -- (ExistsNat (a_sing :: Singleton a)) <- (arbitrary :: Gen (Exists Singleton))
    (ExistsNat (a_sing :: Singleton a)) <- genMinBoundedSingleton (List.length ds_ns) -- We generate at least n actions where n = number of domains
    let ss_ns = allNS s_sing
    let as_ns = allNS a_sing
    let init = List.head ss_ns
    -- init  <- elements ss_ns
    as_prelim  <- randomPick' as_ns (List.length ds_ns)
    let dom_prelim = zip ds_ns as_prelim -- Pre-assign some actions; one each for each security domain
    let as_rest = as_ns List.\\ as_prelim
    dom_rest <- (\a -> do d <- elements ds_ns; return (d, a)) `mapM` as_rest
    let dom = dom_prelim ++ dom_rest
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

genASIntermediateShrink :: ExistsASIntermediate -> [ExistsASIntermediate]
genASIntermediateShrink (ExAI (ss_ns, ds_ns, as_ns, dom, trans, obser, inter, init)) =
    let new_ss_ns_candidates = shrink ss_ns in
    let new_ds_ns_candidates = shrink ds_ns in
    let new_as_ns_candidates = shrink as_ns in
    map (\(new_ss_ns, new_ds_ns, new_as_ns) ->
        let new_dom   = List.filter (\(d, a) -> List.elem d new_ds_ns && List.elem a new_as_ns) dom in
        let new_trans = Map.filterWithKey (\(from, action) to -> (List.elem from new_ss_ns && List.elem action new_as_ns) && List.all (\s -> List.elem s new_ss_ns) to) trans in
        let new_obser = Map.filterWithKey (\(s, d) _ -> List.elem s new_ss_ns && List.elem d new_ds_ns) obser in
        let new_inter = List.filter (\(d1, d2) -> List.elem d1 new_ds_ns && List.elem d2 new_ds_ns) inter in
        let new_init  = List.head new_ss_ns in
        ExAI (new_ss_ns, new_ds_ns, new_as_ns, new_dom, new_trans, new_obser, new_inter, new_init)
    ) $ allTrios new_ss_ns_candidates new_ds_ns_candidates new_as_ns_candidates

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

showNSPrefix :: String -> NatSet n -> String
showNSPrefix prefix val = prefix ++ show (natToInt $ nsToNat val)

buildAS :: ASIntermediate s a d -> System s a d
buildAS (ss, ds, as, dom, trans, obser, inter, init) = System {
    initial     = init,
    step        = as_step trans,
    obs         = as_obs obser,
    dom         = as_dom dom,
    policy      = asBuildPolicy inter,
    actions     = as,
    show_state  = showNSPrefix "s",
    show_action = showNSPrefix "a",
    show_domain = showNSPrefix "d"
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

genASSystemShrink :: ExistsASSystem -> [ExistsASSystem]
genASSystemShrink (ExAS sys) =
    let intermediate = ExAI $ getASIntermediate sys in
    map buildASExists $ genASIntermediateShrink intermediate

instance Arbitrary ExistsASSystem where
    arbitrary = genASSystem Nothing

