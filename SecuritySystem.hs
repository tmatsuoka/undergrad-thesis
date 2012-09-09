-- Rushby's finite system (essentially DFA/NFA)

module SecuritySystem where

import Data.List
import SecurityBasics

run :: System s a o p -> s -> [a] -> s
run _ s [] = s
run sys s (a:as) = run sys ((step sys) s a) as

do_run :: System s a o p -> [a] -> s
do_run sys as = run sys (initial sys) as

getActions :: System s a o p -> [a]
getActions sys = action_list sys

{-
This permutation function isn't right.
We need to do the following to fix this:
1) Generate all states
2) For every state find out [Action] which is required to reach from S0 -> that state
   --> Dijkstra's?
3) Return resulting [[Action]]. Security definitions can use this to check
   system for all states * domain combinations (which is how obs function is defined)
-}
getActionPermutation :: System s a o p -> [[a]]
getActionPermutation sys = concatMap permutations $ subsequences $ getActions sys

-- test :: System s a o p -> [a] -> a -> o
-- test sys as a = obs sys (do_run sys as) a

{-
data Domain = Domain { id_d :: Char }

purge :: System -> (Domain -> Domain -> Bool) -> [Action] -> Domain -> [Action]
purge sys inter (a:as) v | ((dom sys) a) `inter` v = a : (purge sys inter as v)
                         | otherwise               = purge sys inter as v

partial_secure :: System -> (Domain -> Domain -> Bool) -> [Action] -> Action -> Bool
partial_secure sys inter as a = test sys as a == test sys (purge sys inter as ((dom sys) a)) a
-}

