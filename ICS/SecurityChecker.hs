{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

{-

Thesis main dish: Non-interference checker.

-}

module ICS.Checker where

import Debug.Trace

import qualified Data.List as List
import Data.Maybe
import Test.QuickCheck

import Singletons
import SecurityBasics
import DefaultSecurityDefinitions
import ArbitrarySystem

import qualified SSDLLite as EDSL

-- Partitions a list into multiple lists, each belonging in an equivalence class of user's choice (equality test supplied as a function).
-- Similar to Data.List's group/groupBy, but elements (in the same equivalence class) don't have to be next to each other.

split_equiv :: (a -> a -> Bool) -> [a] -> [[a]]
split_equiv test_f xs = split_equiv' test_f xs []
    where split_equiv' :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
          split_equiv' _      []     partials = partials
          split_equiv' test_f (x:xs) partials =                          -- take first element
              let (equivs, non_equivs) = List.partition (test_f x) xs in -- split the rest of list into equivalent/non-equivalent parts
              let this_class = x:equivs in
              split_equiv' test_f non_equivs (this_class:partials)       -- (x:equivs) is now one equivalence class. Split the rest.

prop_purge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_purge_secure sys list =
    let obs_f = obs sys in
    List.all (\d ->
        let purge_equivs = split_equiv (purge_test sys d) list in
        List.all (\equiv_class ->
            -- For this purge_equivalence class, generate observation equivalences
            let obs_equivs = split_equiv (\as1 as2 -> let state1 = doRun sys as1 in
                                                      let state2 = doRun sys as2 in
                                                      obs_f (fromJust state1) d == obs_f (fromJust state2) d
                                         ) equiv_class in
            -- If this is P-secure then we should get [obs-equiv] == P-equiv.
            (List.head obs_equivs) == equiv_class
        ) purge_equivs
    ) $ allNS (value :: GenSingleton d => Singleton d)
    where purge_test :: System s a d -> Domain d -> [Action a] -> [Action a] -> Bool
          purge_test sys d as1 as2 = let state1 = doRun sys as1 in
                                     let state2 = doRun sys as2 in
                                     if (isJust state1 && isJust state2) then
                                         purge sys (policy sys) as1 d == purge sys (policy sys) as2 d
                                     else
                                         True -- TODO: this shouldn't be true. If we get this it means system isn't input-enabled.

prop_ipurge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_ipurge_secure sys list =
    let obs_f = obs sys in
    List.all (\d ->
        let ipurge_equivs = split_equiv (ipurge_test sys d) list in
        List.all (\equiv_class ->
            -- For this purge_equivalence class, generate observation equivalences
            let obs_equivs = split_equiv (\as1 as2 -> let state1 = doRun sys as1 in
                                                      let state2 = doRun sys as2 in
                                                      obs_f (fromJust state1) d == obs_f (fromJust state2) d
                                         ) equiv_class in
            -- If this is IP-secure then we should get [obs-equiv] == IP-equiv.
            (List.head obs_equivs) == equiv_class
        ) ipurge_equivs
    ) $ allNS (value :: GenSingleton d => Singleton d)
    where ipurge_test :: System s a d -> Domain d -> [Action a] -> [Action a] -> Bool
          ipurge_test sys d as1 as2 = let state1 = doRun sys as1 in
                                      let state2 = doRun sys as2 in
                                      if (isJust state1 && isJust state2) then
                                          ipurge sys (policy sys) as1 d == purge sys (policy sys) as2 d
                                      else
                                          True -- TODO: this shouldn't be true. If we get this it means system isn't input-enabled.

-- getCase: Given a system, generates a random security domain and action sequence.

getCase :: (GenSingleton a) => System s a d -> Gen (Maybe [Action a])
getCase sys = do
    maybeActions <- suchThatMaybe (listOf1 $ (arbitrary :: GenSingleton a => Gen (NatSet a))) (\as -> not $ isNothing $ doRun sys as)
    case maybeActions of
        Just actions -> return $ Just actions
        Nothing      -> return $ Nothing

getCases :: (GenSingleton a) => System s a d -> Gen [[Action a]]
getCases sys = do
    cases <- vectorOf 500 $ getCase sys -- Generate maximum 500 action sequences per system
    return $ List.nub $ (catMaybes cases)

-- Test property which checks that all P-secure systems are IP-secure. Rushby and van der Meyden proved this is true, so QuickCheck should agree.

prop_p_then_ip :: ExistsASSystem -> Property
prop_p_then_ip (ExAS sys) =
    let system = getASBase sys in
    forAll (getCases system) $ (\list -> prop_purge_secure system list ==> prop_ipurge_secure system list)

-- Test property which checks that all IP-secure systems are P-secure. Clearly this is false.

prop_ip_then_p :: ExistsASSystem -> Property
prop_ip_then_p (ExAS sys) =
    let system = getASBase sys in
    forAll (getCases system) $ (\list -> prop_ipurge_secure system list ==> prop_purge_secure system list)

-- Run prop_p_then_ip and prop_ip_then_p should convince you that P ==> IP but IP =/=> P. In other words this shows that P is strictly more secure than IP!

