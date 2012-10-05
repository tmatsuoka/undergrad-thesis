module DefaultSecurityDefinitions where

import qualified Data.List as List

import SecurityBasics
import qualified SSDLLite as EDSL

-- Transitive Purge

purge :: System s a d -> Policy d -> [Action a] -> Domain d -> [Action a]
purge sys p actions u = List.filter (\x -> inter p ((dom sys) x) u) actions


purgeEDSL :: EDSL.ExistsEDSLSystem -> [EDSL.ActionId] -> EDSL.DomainId -> [EDSL.ActionId]
purgeEDSL (EDSL.ExES sys) as_id d_id = map (\a -> EDSL.lookupId a as_ns) purged
    where (_, ds_ns, as_ns, _, _, _, _, _) = EDSL.getIntermediate sys
          as = map (EDSL.genThing as_ns) as_id
          d  = EDSL.genThing ds_ns d_id
          base_sys = EDSL.getBase sys
          purged = purge base_sys (policy base_sys) as d

-- Intransitive Purge

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

ipurgeEDSL :: EDSL.ExistsEDSLSystem -> [EDSL.ActionId] -> EDSL.DomainId -> [EDSL.ActionId]
ipurgeEDSL (EDSL.ExES sys) as_id d_id = map (\a -> EDSL.lookupId a as_ns) ipurged
    where (_, ds_ns, as_ns, _, _, _, _, _) = EDSL.getIntermediate sys
          as = map (EDSL.genThing as_ns) as_id
          d  = EDSL.genThing ds_ns d_id
          base_sys = EDSL.getBase sys
          ipurged = ipurge base_sys (policy base_sys) as d

-- TA Security

data TATree a = Branch (TATree a) (TATree a) (Action a)
              | Empty
              deriving (Show, Eq)

ta :: System s a d -> [Action a] -> Domain d -> TATree a
ta sys []      u = Empty
ta sys actions u =
    let as = List.init actions in
    let a  = List.last actions in
    let dom_sys = dom sys in
    let interp  = inter (policy sys) in
    if ((dom_sys a) `interp` u) then
        Branch (ta sys as u) (ta sys as (dom_sys a)) a
    else (ta sys as u)
