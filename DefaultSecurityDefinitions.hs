module DefaultSecurityDefinitions where

import Data.List as List
import SecurityBasics
import SecuritySystem

{- sources :: Policy p => System -> [Action] -> p -> Set p
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
-}

purge :: System s a o d -> Policy d -> [a] -> d -> [a]
purge sys p actions u = List.filter (\x -> inter p ((dom sys) x) u) actions

sources :: System s a o d -> Policy d -> [a] -> d -> [d]
sources sys p []     u = [u]
sources sys p (a:as) u = let next_sources = sources sys p as u
                         in let vs = List.filter (\v -> inter p ((dom sys) a) v) next_sources
                         in if (List.null vs == False)
                                 then (((dom sys) a) : next_sources)
                                 else next_sources

ipurge :: Eq d => System s a o d -> Policy d -> [a] -> d -> [a]
ipurge sys p []     u = []
ipurge sys p (a:as) u = if (List.elem ((dom sys) a) (sources sys p as u))
                            then (a : (ipurge sys p as u))
                            else (ipurge sys p as u)

