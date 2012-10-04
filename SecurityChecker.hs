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

prop_purge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_purge_secure sys list =
    List.all (\as ->
        List.all (\d ->
            let obs_f  = obs sys in
            let purged = purge sys (policy sys) as d in
            let other  = List.find (\as2 -> (as /= as2) && (purged == (purge sys (policy sys) as2 d))) list in
            case other of
                Just as2 -> let state1 = doRun sys as  in
                            let state2 = doRun sys as2 in
                            if (isJust state1 && isJust state2) then
                                obs_f (fromJust state1) d == obs_f (fromJust state2) d
                            else True
                Nothing  -> True
        ) $ allNS (value :: GenSingleton d => Singleton d)
    ) list

prop_ipurge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_ipurge_secure sys list =
    List.all (\as ->
        List.all (\d ->
            let obs_f  = obs sys in
            let purged = ipurge sys (policy sys) as d in
            let other  = List.find (\as2 -> (as /= as2) && (purged == (ipurge sys (policy sys) as2 d))) list in
            case other of
                Just as2 -> let state1 = doRun sys as  in
                            let state2 = doRun sys as2 in
                            if (isJust state1 && isJust state2) then
                                obs_f (fromJust state1) d == obs_f (fromJust state2) d
                            else True
                Nothing  -> True
        ) $ allNS (value :: GenSingleton d => Singleton d)
    ) list

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

