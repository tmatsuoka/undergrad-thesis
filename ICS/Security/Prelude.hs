------------------------------------
--
-- ICS.Security.Prelude
--
-- Defines a set of common security definitions, along with corresponding QuickCheck properties to verify them.
--
------------------------------------

module ICS.Security.Prelude where

import ICS.TypeNats
import ICS.System.Basics
import ICS.System.Description
import ICS.Utility

import Data.List

-- Types

prop_secure_stub :: (System s a d -> Domain d -> [Action a] -> [Action a] -> Bool) ->
                    (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_secure_stub equiv_test sys list =
    all (\d ->
        let equivs = split_equiv (equiv_test sys d) list in
        all (obsEq sys d) equivs
    ) $ allNS (value :: GenSingleton d => Singleton d)

-- Transitive Purge

purge :: System s a d -> [Action a] -> Domain d -> [Action a]
purge sys actions u = filter (\x -> inter (policy sys) ((dom sys) x) u) actions

purgeEDSL :: ExistsEDSLSystem -> [ActionId] -> DomainId -> [ActionId]
purgeEDSL (ExES sys) as_id d_id = map (\a -> lookupId a as_ns) purged
    where (_, ds_ns, as_ns, _, _, _, _, _) = getEDSLIntermediate sys
          as = map (genThing as_ns) as_id
          d  = genThing ds_ns d_id
          base_sys = getEDSLBase sys
          purged = purge base_sys as d

prop_purge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_purge_secure sys list = prop_secure_stub (\system d as1 as2 -> purge system as1 d == purge system as2 d) sys list

-- Intransitive Purge

sources :: System s a d -> [Action a] -> Domain d -> [Domain d]
sources sys []     u = [u]
sources sys (a:as) u = let da = (dom sys) a in
                       let interp = inter $ policy sys in
                       let next_sources = sources sys as u in
                       if (any (\v -> da `interp` v) next_sources) then
                          (da : next_sources)
                       else
                           next_sources

ipurge :: System s a d -> [Action a] -> Domain d -> [Action a]
ipurge sys []     u = []
ipurge sys (a:as) u = let da = (dom sys) a in
                      let next_ipurge = ipurge sys as u in
                      if (elem da $ sources sys (a:as) u) then
                          (a : next_ipurge)
                      else
                          next_ipurge

ipurgeEDSL :: ExistsEDSLSystem -> [ActionId] -> DomainId -> [ActionId]
ipurgeEDSL (ExES sys) as_id d_id = map (\a -> lookupId a as_ns) ipurged
    where (_, ds_ns, as_ns, _, _, _, _, _) = getEDSLIntermediate sys
          as = map (genThing as_ns) as_id
          d  = genThing ds_ns d_id
          base_sys = getEDSLBase sys
          ipurged = ipurge base_sys as d

prop_ipurge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_ipurge_secure sys list = prop_secure_stub (\system d as1 as2 -> ipurge system as1 d == ipurge system as2 d) sys list

-- TA Security

data TATree a = Branch (TATree a) (TATree a) (Action a)
              | Empty
              deriving (Show, Eq)

ta :: System s a d -> [Action a] -> Domain d -> TATree a
ta sys []      u = Empty
ta sys actions u =
    let as = init actions in
    let a  = last actions in
    let dom_sys = dom sys in
    let interp  = inter (policy sys) in
    if ((dom_sys a) `interp` u) then
        Branch (ta sys as u) (ta sys as (dom_sys a)) a
    else (ta sys as u)

prop_ta_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_ta_secure sys list = prop_secure_stub (\system d as1 as2 -> ta system as1 d == ta system as2 d) sys list
