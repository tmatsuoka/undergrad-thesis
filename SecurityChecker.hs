{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

{-

Thesis "kid's meal": Non-interference checker.

-}

module SecurityChecker where

import Debug.Trace

import qualified Data.List as List
import Data.Maybe
import Test.QuickCheck

import Singletons
import SecurityBasics
import DefaultSecurityDefinitions
import ArbitrarySystem

import qualified SSDLLite as EDSL

prop_purge_secure :: System s a d -> [(Domain d, [Action a])] -> [(Domain d, [Action a])] -> Bool
prop_purge_secure sys list1 list2 =
    List.all (\t1@(d, as) ->
        let obs_f  = obs sys in
        let purged = purge sys (policy sys) as d in
        let other  = List.find (\t2@(_, as2) -> (as /= as2) && (purged == (purge sys (policy sys) as2 d))) list2 in
        case other of
            Just (_, as2) -> let state1 = doRun sys as  in
                             let state2 = doRun sys as2 in
                             -- TODO: Do not assume state1 and state2 will not be Nothing!
                             obs_f (fromJust state1) d == obs_f (fromJust state2) d
            Nothing       -> True
    ) list1

prop_ipurge_secure :: System s a d -> [(Domain d, [Action a])] -> [(Domain d, [Action a])] -> Bool
prop_ipurge_secure sys list1 list2 =
    List.all (\t1@(d, as) ->
        let obs_f  = obs sys in
        let purged = ipurge sys (policy sys) as d in
        let other  = List.find (\t2@(_, as2) -> (as /= as2) && (purged == (ipurge sys (policy sys) as2 d))) list2 in
        case other of
            Just (_, as2) -> let state1 = doRun sys as  in
                             let state2 = doRun sys as2 in
                             -- TODO: Do not assume state1 and state2 will not be Nothing!
                             obs_f (fromJust state1) d == obs_f (fromJust state2) d
            Nothing       -> True
    ) list1

-- getCase: Given a system, generates a random security domain and action sequence.

getCase :: (GenSingleton a, GenSingleton d) => System s a d -> Gen (Maybe (Domain d, [Action a]))
getCase sys = do
    domain  <- arbitrary :: GenSingleton d => Gen (NatSet d)
    maybeActions <- suchThatMaybe (listOf1 $ (arbitrary :: GenSingleton a => Gen (NatSet a))) (\as -> not $ isNothing $ doRun sys as)
    case maybeActions of
        Just actions -> return $ Just (domain, actions)
        Nothing      -> return $ Nothing

getCases :: (GenSingleton a, GenSingleton d) => System s a d -> Gen [(Domain d, [Action a])]
getCases sys = do
    cases <- vectorOf 50 $ getCase sys -- Generate maximum 100 action sequences per system
    return (catMaybes cases)

-- Test property which checks that all P-secure systems are IP-secure. Rushby and van der Meyden proved this is true, so QuickCheck should agree.

prop_p_then_ip :: ExistsASSystem -> Property
prop_p_then_ip (ExAS sys) =
    let system = getASBase sys in
    forAll (do
        list1a <- getCases system
        list1b <- getCases system
        list2a <- getCases system
        list2b <- getCases system
        return (list1a, list1b, list2a, list2b)
    ) $ (\(list1a, list1b, list2a, list2b) -> prop_purge_secure system list1a list1b ==> prop_ipurge_secure system list2a list2b)

-- Test property which checks that all IP-secure systems are P-secure. Clearly this is false.

prop_ip_then_p :: ExistsASSystem -> Property
prop_ip_then_p (ExAS sys) =
    let system = getASBase sys in
    forAll (do
        list1a <- getCases system
        list1b <- getCases system
        list2a <- getCases system
        list2b <- getCases system
        return (list1a, list1b, list2a, list2b)
    ) $ (\(list1a, list1b, list2a, list2b) -> prop_ipurge_secure system list1a list1b ==> prop_purge_secure system list2a list2b)

-- Run prop_p_then_ip and prop_ip_then_p should convince you that P ==> IP but IP =/=> P. In other words this shows that P is strictly more secure than IP!

