{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleInstances #-}

{-

Thesis main dish: Non-interference checker.

-}

module ICS.Checker where

import ICS.TypeNats
import ICS.System.Basics
import ICS.System.Arbitrary
import ICS.System.Description
import ICS.Security.Prelude

import Data.List
import Test.QuickCheck

-- We need Rank-N types to express this.
type SecureProperty = forall s. forall a. forall d. (GenSingleton a, GenSingleton d) => System s a d -> [[Action a]] -> Bool

-- getCase: Given a system, generates a random security domain and action sequence.

getCase :: (GenSingleton a) => System s a d -> Gen ([Action a])
getCase sys =
    listOf1 $ (arbitrary :: GenSingleton a => Gen (NatSet a))

getCases :: (GenSingleton a) => System s a d -> Gen [[Action a]]
getCases sys = do
    cases <- vectorOf 50 $ getCase sys -- Generate maximum 50 action sequences per system
    return $ nub cases

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

verify' :: (GenSingleton a, GenSingleton d) => System s a d -> (System s a d -> [[Action a]] -> Bool) -> IO ()
verify' sys prop = quickCheck $ forAll (getCases sys) $ (\list -> prop sys list)

-- verify: Given an EDSL system, checks if the system satisfies the given security definition.
-- verify :: (forall s. forall a. forall d. (GenSingleton a, GenSingleton d) => System s a d -> [[Action a]] -> Bool) -> ExistsEDSLSystem -> IO ()
verify :: ExistsEDSLSystem -> SecureProperty -> IO ()
verify (ExES (ES sys _)) prop = verify' sys prop

prop_imply :: SecureProperty -> SecureProperty -> ExistsASSystem -> Property
prop_imply first second (ExAS (AS sys _)) =
    forAll (getCases sys) $ (\list -> first sys list ==> second sys list)

separate :: SecureProperty -> SecureProperty -> Maybe ExistsASPolicy -> IO ()
separate first second policy = quickCheck $ forAll (genASSystem policy) $ (\sys -> prop_imply first second sys)

