module ICS.Utility where

import Data.List

applyFirst :: (a -> b) -> (a, c) -> (b, c)
applyFirst f (a, c) = (f a, c)

-- Make all possible pairs of the elements in the two lists

allPairs :: [a] -> [b] -> [(a, b)]
allPairs as bs = concatMap (\a -> map (\b -> (a, b)) bs) as

splitTupleLines :: (Show a, Show b) => [(a, b)] -> String
splitTupleLines list =
    foldl (\a b -> a ++ "\n" ++ b) ""
    (map (\(a, b) -> "(" ++ show a ++ ", " ++ show b ++ ")") list)
