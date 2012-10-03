{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

{-

Thesis "kid's meal": Non-interference checker.

-}

module SecurityChecker where

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
    not (isNothing state) ==> not (isNothing purged_state) ==> obs_f (fromJust state) d == obs_f (fromJust purged_state) d
    where state        = doRun sys as
          purged_state = doRun sys (purge sys (policy sys) as d)
          obs_f        = obs sys

-- securityCheckEDSL :: EDSL.ExistsEDSLSystem -> IO ()
securityCheckEDSL (EDSL.ExES sys) = quickCheck (prop_purge_secure b_sys)
    where b_sys = EDSL.getBase sys

