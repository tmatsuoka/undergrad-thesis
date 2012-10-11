------------------------------------
--
-- QuickSCS.Security.Prelude
--
-- Defines a set of common security definitions, along with corresponding QuickCheck properties to verify them.
--
------------------------------------

module QuickSCS.Security.Prelude where

import QuickSCS.TypeNats
import QuickSCS.System.Basics
import QuickSCS.System.Description hiding (obs)
import QuickSCS.Utility

import Data.List
import Data.Either
import Test.QuickCheck.Property

-- Types

prop_secure_stub :: (System s a d -> Domain d -> [Action a] -> [Action a] -> Bool) ->
                    (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_secure_stub equiv_test sys list =
    all (\d ->
        let equivs = split_equiv (equiv_test sys d) list in
        all (obsEq sys d) equivs
    ) $ allNS (value :: GenSingleton d => Singleton d)

prop_secure_single_stub :: (System s a d -> Domain d -> [Action a] -> [Action a]) ->
                           (GenSingleton d) => System s a d -> [[Action a]] -> Result
prop_secure_single_stub purger sys list =
    let obsers = concatMap (\d ->
                            map (\as ->
                                let orig_obs = map (\s -> (obs sys) s d) $ doRun sys as in
                                let purged_obs = map (\s -> (obs sys) s d) $ doRun sys (purger sys d as) in
                                if (orig_obs == purged_obs) then
                                    Right True
                                else
                                    Left (d, as, orig_obs, purged_obs)
                            ) list
                     ) $ allNS (value :: GenSingleton d => Singleton d) in
    let (wrongs, rights) = partitionEithers obsers in
    if (not $ null wrongs) then
        case head wrongs of
        (fail_d, fail_as, fail_obs, fail_purged) ->
            MkResult {
                ok = Just False,
                expect = True,
                reason = ("Security check failed:\nFor this domain \"" ++ (show_domain sys) fail_d ++ "\"" ++
                          " and actions " ++ (show $ map (show_action sys) fail_as) ++ ":\n" ++
                          "Original observation is " ++ show fail_obs ++ ", but after purge it is " ++ show fail_purged),
                interrupted = False,
                stamp = [],
                callbacks = []
            }
    else
        MkResult {
            ok = Just True,
            expect = True,
            reason = "",
            interrupted = False,
            stamp = [],
            callbacks = []
        }

obsEqRes sys d as = obsEqRes' sys d as Nothing
    where obs_f = obs sys
          obsEqRes' sys d []     _                       = Right True
          obsEqRes' sys d (a:as) Nothing                 = let a_obs = obs_f (head $ doRun sys a) d in
                                                           obsEqRes' sys d as (Just (a, a_obs))
          obsEqRes' sys d (a:as) (Just (prev, prev_obs)) = let a_obs = obs_f (head $ doRun sys a) d in
                                                           if (prev_obs == a_obs) then
                                                               obsEqRes' sys d as (Just (a, a_obs))
                                                           else
                                                               Left (d, prev, prev_obs, a, a_obs)

prop_secure_map_stub :: (Eq b, Ord b) => (System s a d -> Domain d -> [Action a] -> b) ->
                    (GenSingleton d) => System s a d -> [[Action a]] -> Result
prop_secure_map_stub transformer sys list =
    let obsers = concatMap (\d ->
                            let equivs = split_equiv_map (transformer sys d) list in
                            map (obsEqRes sys d) equivs
                           ) $ allNS (value :: GenSingleton d => Singleton d) in
    let (wrongs, rights) = partitionEithers obsers in
    if (not $ null wrongs) then
        case head wrongs of
        (fail_d, fail1, fail_obs1, fail2, fail_obs2) ->
            MkResult {
                ok = Just False,
                expect = True,
                reason = ("Security check failed:\nFor this domain \"" ++ (show_domain sys) fail_d ++ "\"" ++
                          " and actions " ++ (show $ map (show_action sys) fail1) ++ " and " ++ (show $ map (show_action sys) fail2) ++ ":\n" ++
                          "Observations are different; respectively they are " ++ show fail_obs1 ++ " and " ++ show fail_obs2),
                interrupted = False,
                stamp = [],
                callbacks = []
            }
    else
        MkResult {
            ok = Just True,
            expect = True,
            reason = "",
            interrupted = False,
            stamp = [],
            callbacks = []
        }

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

prop_purge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Result
-- prop_purge_secure sys list = prop_secure_stub (\system d as1 as2 -> purge system as1 d == purge system as2 d) sys list
prop_purge_secure sys list = prop_secure_single_stub (\system d as -> purge system as d) sys list

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

prop_ipurge_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Result
-- prop_ipurge_secure sys list = prop_secure_stub (\system d as1 as2 -> ipurge system as1 d == ipurge system as2 d) sys list
-- prop_ipurge_secure sys list = prop_secure_map_stub (\system d as -> ipurge system as d) sys list
prop_ipurge_secure sys list = prop_secure_single_stub (\system d as -> ipurge system as d) sys list

-- TA Security

data TATree a = Branch (TATree a) (TATree a) (Action a)
              | Empty
              deriving (Show, Eq, Ord)

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

prop_ta_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Result
-- prop_ta_secure sys list = prop_secure_stub (\system d as1 as2 -> ta system as1 d == ta system as2 d) sys list
prop_ta_secure sys list = prop_secure_map_stub (\system d as -> ta system as d) sys list
