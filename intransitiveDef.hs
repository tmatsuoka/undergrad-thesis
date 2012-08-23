import Data.Set as Set
import SecuritySystem

sources :: Policy p => System -> [Action] -> p -> Set p
sources _ [] u     = Set.Singleton u
sources sys (a:as) = let vs = Set.filter (\x -> ((dom sys) a) `inter` v) (sources sys inter as u)
                     in
                         if (!Set.null vs)
                             then (sources sys as u) `Set.union` ((dom sys) a)
                             else (sources sys as u)

-- ipurge
ipurge :: Policy p => System -> [Action] -> p -> [Action]
ipurge sys [] u = []
ipurge sys actions@(a:as) u = let ss = Set.filter (\x -> x == (dom sys) x) (sources sys actions u)
                              in
                                  if (!Set.null ss)
                                    then a : ipurge sys as u
                                    else ipurge sys as u

-- We probably need unwinding theorem here, otherwise we can't check whether the system satisfies the definition.

