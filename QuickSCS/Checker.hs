{-# LANGUAGE RankNTypes, ScopedTypeVariables, FlexibleInstances #-}

{-

Thesis main dish: Non-interference checker.

-}

module QuickSCS.Checker where

import QuickSCS.TypeNats
import QuickSCS.Utility
import QuickSCS.System.Basics
import QuickSCS.System.Arbitrary
import QuickSCS.System.Description hiding (actions)
import QuickSCS.Security.Prelude

import Control.Exception
import Control.Monad
import Data.List
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property

-- We need Rank-N types to express this.
type SecureProperty = forall s. forall a. forall d. (GenSingleton a, GenSingleton d) => System s a d -> [[Action a]] -> Result

-- getCase: Given a system, generates a random security domain and action sequence.

getCase :: (GenSingleton a) => System s a d -> Gen ([Action a])
getCase sys =
    listOf $ (arbitrary :: GenSingleton a => Gen (NatSet a))

getCases :: (GenSingleton a) => System s a d -> Gen [[Action a]]
getCases sys = do
    cases   <- liftM nub $ vectorOf 2000 $ getCase sys
    return cases
    -- let obsers = split_equiv_map (doRun sys) cases
    -- return $ map (head) obsers

{-
asSplit :: [Action a] -> [[Action a]]
asSplit as = map (\a -> [a]) as

getCases :: System s a d -> Int -> [[Action a]]
getCases sys length | length == 0 = []
                    | length == 1 = asSplit $ actions sys
                    | otherwise   = let as = actions sys in (asSplit as) ++ (concatMap (\xs -> map (\y -> xs ++ [y]) as) $ getCases sys (length - 1))
-}

getCasesShrink :: (GenSingleton a) => [[Action a]] -> [[[Action a]]]
getCasesShrink a_seqs = map (shrink) a_seqs

args :: Args
args = Args {
    replay          = Nothing,
    maxSuccess      = 100000,
    maxDiscardRatio = 1000000,
    maxSize         = 15,
    chatty          = True
    }

verify' :: (GenSingleton a, GenSingleton d) => System s a d -> (System s a d -> [[Action a]] -> Result) -> IO ()
verify' sys prop = quickCheckWith args $ forAll (getCases sys) $ (\list -> prop sys list)

-- verify: Given an EDSL system, checks if the system satisfies the given security definition.
verify :: ExistsEDSLSystem -> SecureProperty -> IO ()
verify (ExES (ES sys _)) prop = verify' sys prop
-- verify (ExES (ES sys intermediate)) prop = quickCheckWith args (prop sys (getCases sys 10))

prop_all :: SecureProperty -> ExistsASSystem -> Property
prop_all prop (ExAS (AS sys _)) =
    forAll (getCases sys) (\list -> prop sys list)

prop_imply :: SecureProperty -> SecureProperty -> ExistsASSystem -> Property
prop_imply first second (ExAS (AS sys _)) =
    forAll (getCases sys) (\list -> let first_res = first sys list in
                                    case first_res of
                                        MkResult { ok = Just True } ->
                                            let second_res = second sys list in
                                            case second_res of
                                                MkResult { ok = Just False } -> assert (ok first_res == Just True) second_res
                                                otherwise                    -> assert (ok second_res /= Just False) second_res
                                        otherwise                   -> rejected)
--     forAll (getCases sys) (\list -> first sys list .&&. second sys list)

-- let list = getCases sys 10 in first sys list ==> second sys list

falsifySample :: SecureProperty -> Maybe ExistsASPolicy -> IO ()
falsifySample prop policy = quickCheckWith args $ forAll (genASSystem policy) (\sys -> prop_all prop sys)

separate :: SecureProperty -> SecureProperty -> Maybe ExistsASPolicy -> IO ()
separate first second policy = do
    quickCheckWith args $ forAll (genASSystem policy) (\sys -> prop_imply first second sys)
    quickCheckWith args $ forAll (genASSystem policy) (\sys -> prop_imply second first sys)
