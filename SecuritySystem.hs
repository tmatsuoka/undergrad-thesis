-- Rushby's finite system (essentially DFA/NFA)

module SecuritySystem where

import SecurityBasics

run :: System -> State -> [Action] -> State
run _ s [] = s
run sys s (a:as) = run sys ((step sys) s a) as

do_run :: System -> [Action] -> State
do_run sys as = run sys (initial sys) as

test :: System -> [Action] -> Action -> Output
test sys as a = output sys (do_run sys as) a

{-
data Domain = Domain { id_d :: Char }

purge :: System -> (Domain -> Domain -> Bool) -> [Action] -> Domain -> [Action]
purge sys inter (a:as) v | ((dom sys) a) `inter` v = a : (purge sys inter as v)
                         | otherwise               = purge sys inter as v

partial_secure :: System -> (Domain -> Domain -> Bool) -> [Action] -> Action -> Bool
partial_secure sys inter as a = test sys as a == test sys (purge sys inter as ((dom sys) a)) a
-}

