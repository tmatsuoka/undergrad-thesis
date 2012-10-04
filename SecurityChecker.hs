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

prop_purge_secure :: System s a d -> Domain d -> [Action a] -> Bool
prop_purge_secure sys d as =
    if (isNothing purged_state) then
        True
    else obs_f (fromJust state) d == obs_f (fromJust purged_state) d
        where state        = doRun sys as
              purged_state = doRun sys (purge sys (policy sys) as d)
              obs_f        = obs sys

prop_ipurge_secure :: System s a d -> Domain d -> [Action a] -> Bool
prop_ipurge_secure sys d as =
    if (isNothing purged_state) then
        True
    else obs_f (fromJust state) d == obs_f (fromJust purged_state) d
        where state        = doRun sys as
              purged_state = doRun sys (ipurge sys (policy sys) as d)
              obs_f        = obs sys

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
    cases <- vectorOf 100 $ getCase sys -- Generate maximum 100 action sequences per system
    return (catMaybes cases)

-- allWell: Property which checks prop1 ==> prop2 (but returning Bool instead of Property)

allWell :: (GenSingleton a, GenSingleton d) =>
           System s a d ->
           [(Domain d, [Action a])] ->
           (System s a d -> Domain d -> [Action a] -> Bool) -> 
           (System s a d -> Domain d -> [Action a] -> Bool) -> 
           Bool
allWell sys list prop1 prop2 = 
    let result = List.filter (\(d, as) -> not (if not (prop1 sys d as) then True else prop2 sys d as)) list in
    if (List.null result) then
        True
    else
        let (d, as) = List.head result in
        trace ("Test failed, sequence: " ++ show d ++ ", " ++ show as) False

-- Test property which checks that all P-secure systems are IP-secure. Rushby and van der Meyden proved this is true, so QuickCheck should agree.

prop_p_then_ip :: ExistsASSystem -> Property
prop_p_then_ip (ExAS sys) =
    let system = getASBase sys in
    forAll (getCases system) $ (\list -> allWell system list prop_purge_secure prop_ipurge_secure)

-- Test property which checks that all IP-secure systems are P-secure. Clearly this is false.

prop_ip_then_p :: ExistsASSystem -> Property
prop_ip_then_p (ExAS sys) =
    let system = getASBase sys in
    forAll (getCases system) $ (\list -> allWell system list prop_ipurge_secure prop_purge_secure)

-- Run prop_p_then_ip and prop_ip_then_p should convince you that P ==> IP but IP =/=> P. In other words this shows that P is strictly more secure than IP!

