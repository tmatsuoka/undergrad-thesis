------------------------------------
--
-- QuickSCS.Utility
--
-- Auxiliary functions that are used in other modules.
--
------------------------------------

module QuickSCS.Utility where

import Debug.Trace
import QuickSCS.System.Basics
import Data.List

-- Applies a function on the first element of a tuple.

applyFirst :: (a -> b) -> (a, c) -> (b, c)
applyFirst f (a, c) = (f a, c)

-- Make all possible pairs of the elements in the two lists.

allPairs :: [a] -> [b] -> [(a, b)]
allPairs as bs = concatMap (\a -> map (\b -> (a, b)) bs) as

allTrios :: [a] -> [b] -> [c] -> [(a, b, c)]
allTrios as bs cs = concatMap (\a -> concatMap (\b -> map (\c -> (a, b, c)) cs) bs) as

splitTupleLines :: (Show a, Show b) => [(a, b)] -> String
splitTupleLines list =
    foldl (\a b -> a ++ "\n" ++ b) ""
    (map (\(a, b) -> "(" ++ show a ++ ", " ++ show b ++ ")") list)

-- Partitions a list into multiple lists, each belonging in an equivalence class of user's choice (equality test supplied as a function).
-- Similar to Data.List's group/groupBy, but elements (in the same equivalence class) don't have to be next to each other.

split_equiv :: (a -> a -> Bool) -> [a] -> [[a]]
split_equiv test_f xs = split_equiv' test_f xs []
    where split_equiv' :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
          split_equiv' _      []     partials = partials
          split_equiv' test_f (x:xs) partials =                     -- take first element
              let (equivs, non_equivs) = partition (test_f x) xs in -- split the rest of list into equivalent/non-equivalent parts
              let this_class = x:equivs in
              split_equiv' test_f non_equivs (this_class:partials)  -- (x:equivs) is now one equivalence class. Split the rest.

split_equiv_map :: (Show a, Show b, Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
split_equiv_map transformer xs =
    let transformed = map (\x -> (x, transformer x)) xs in
    let transformed_group = groupBy (\(_, y1) (_, y2) -> y1 == y2) $ sortBy (\(_, y1) (_, y2) -> compare y1 y2) transformed in
    -- traceShow transformed_group $ map (\group -> map fst group) transformed_group
    map (\group -> map fst group) transformed_group

-- Given domain and list of action sequences, check if all actions will result in the same observation(s).

obsEq :: System s a d -> Domain d -> [[Action a]] -> Bool
obsEq sys d as =
    let obs_f = obs sys in
    length (nubBy (\xs ys ->
        let xs_obs = map (\s -> obs_f s d) $ doRun sys xs in
        let ys_obs = map (\s -> obs_f s d) $ doRun sys ys in
        null $ (nub xs_obs) \\ (nub ys_obs)
    ) as) == 1
