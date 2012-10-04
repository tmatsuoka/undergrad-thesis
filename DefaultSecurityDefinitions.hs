module DefaultSecurityDefinitions where

import Data.List as List
import SecurityBasics

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

purge :: System s a d -> Policy d -> [Action a] -> Domain d -> [Action a]
purge sys p actions u = List.filter (\x -> inter p ((dom sys) x) u) actions

sources :: System s a d -> Policy d -> [Action a] -> Domain d -> [Domain d]
sources sys p []     u = [u]
sources sys p (a:as) u = let da = (dom sys) a in
                         let interp = inter p in
                         let next_sources = sources sys p as u in
                         if (List.any (\v -> da `interp` v) next_sources) then
                            (da : next_sources)
                         else
                             next_sources

ipurge :: System s a d -> Policy d -> [Action a] -> Domain d -> [Action a]
ipurge sys p []     u = []
ipurge sys p (a:as) u = let da = (dom sys) a in
                        let next_ipurge = ipurge sys p as u in
                        if (List.elem da $ sources sys p (a:as) u) then
                            (a : next_ipurge)
                        else
                            next_ipurge

