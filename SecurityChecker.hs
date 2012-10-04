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

{-
instance (GenSingleton m, GenSingleton n) => Arbitrary (NatSet m, NatSet n) where
    arbitrary = return (arbitrary :: Gen (GenSingleton m => NatSet m), arbitrary :: Gen (NatSet n))
-}

prop_purge_secure :: System s a d -> Domain d -> [Action a] -> Property
prop_purge_secure sys d as = 
    not (List.null as) ==> not (isNothing state) ==> not (isNothing purged_state) ==> obs_f (fromJust state) d == obs_f (fromJust purged_state) d
    where state        = doRun sys as
          purged_state = doRun sys (purge sys (policy sys) as d)
          obs_f        = obs sys

prop_ipurge_secure :: System s a d -> Domain d -> [Action a] -> Property
prop_ipurge_secure sys d as = 
    not (List.null as) ==> not (isNothing state) ==> not (isNothing purged_state) ==> obs_f (fromJust state) d == obs_f (fromJust purged_state) d
    where state        = doRun sys as
          purged_state = doRun sys (ipurge sys (policy sys) as d)
          obs_f        = obs sys

-- securityCheckEDSL :: EDSL.ExistsEDSLSystem -> IO ()
securityCheckEDSL (EDSL.ExES sys) = quickCheck (prop_purge_secure b_sys)
    where b_sys = EDSL.getBase sys

-- getCase: Given a system, generates a random security domain and action sequence.
-- TODO: Make this so that it always gives out executable action sequences.

getCase :: (GenSingleton a, GenSingleton d) => System s a d -> Gen (Domain d, [Action a])
getCase sys = do
    domain  <- arbitrary :: GenSingleton d => Gen (NatSet d)
    actions <- listOf (arbitrary :: GenSingleton a => Gen (NatSet a))
    return (domain, actions)

-- Test property which checks that all systems must be both P-secure and IP-secure. Clearly this is false (run it and see!)

prop_ip_and_p :: ExistsASSystem -> Property
prop_ip_and_p (ExAS sys) =
    let system = getASBase sys in
    forAll (getCase system) $ (\(d, as) -> (prop_purge_secure system d as) .&&. (prop_ipurge_secure system d as))

