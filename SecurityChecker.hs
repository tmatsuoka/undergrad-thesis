{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

{-

Thesis "kid's meal": Non-interference checker.

-}

module SecurityChecker where

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

getCase :: (GenSingleton a, GenSingleton d) => System s a d -> Gen (Domain d, [Action a])
getCase sys = do
    domain  <- arbitrary :: GenSingleton d => Gen (NatSet d)
    actions <- suchThat (listOf1 $ (arbitrary :: GenSingleton a => Gen (NatSet a))) (\as -> not $ isNothing $ doRun sys as)
    return (domain, actions)

getCases :: (GenSingleton a, GenSingleton d) => System s a d -> Gen [(Domain d, [Action a])]
getCases sys = vectorOf 1 $ getCase sys

allWell :: (GenSingleton a, GenSingleton d) =>
           System s a d ->
           [(Domain d, [Action a])] ->
           (System s a d -> Domain d -> [Action a] -> Bool) -> 
           (System s a d -> Domain d -> [Action a] -> Bool) -> 
           Bool
allWell sys list prop1 prop2 = all (\(d, as) -> if not (prop1 sys d as) then True else prop2 sys d as) list

-- Test property which checks that all systems must be both P-secure and IP-secure. Clearly this is false (run it and see!)

prop_ip_and_p :: ExistsASSystem -> Property
prop_ip_and_p (ExAS sys) =
    let system = getASBase sys in
    forAll (getCases system) $ (\list -> allWell system list prop_purge_secure prop_ipurge_secure)

