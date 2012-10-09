import ICS.TypeNats
import ICS.System.Basics
import ICS.System.Description
import ICS.System.Arbitrary
import ICS.Security.Prelude
import ICS.Checker

import Debug.Trace
import Data.Maybe
import qualified Data.List as List
import Test.QuickCheck

ta_policy = do
    domains ["h1", "h2", "d1", "d2", "l"]
    "h1" >-> "d1"
    "h2" >-> "d2"
    "d1" >-> "l"
    "d2" >-> "l"

ta_system = do
    actions "h1" ["a_h1"]
    actions "h2" ["a_h2"]
    actions "d1" ["a_d1"]
    actions "d2" ["a_d2"]
    actions "l"  ["a_l"]

    ("<init>"  , "a_h1") ~> ["s_h1"]
    ("<init>"  , "a_h2") ~> ["s_h2"]
    ("s_h1"    , "a_h2") ~> ["s_h1h2"]
    ("s_h2"    , "a_h1") ~> ["s_h2h1"]
    ("s_h1h2"  , "a_d1") ~> ["s_h1h2d1"]
    ("s_h2h1"  , "a_d1") ~> ["s_h2h1d1"]
    ("s_h1h2d1", "a_d2") ~> ["s_h1h2d1d2"]
    ("s_h2h1d1", "a_d2") ~> ["s_h2h1d1d2"]

    ("<init>", "h1") >? "o_<init>"
    ("<init>", "h2") >? "o_<init>"
    ("<init>", "d1") >? "o_<init>"
    ("<init>", "d2") >? "o_<init>"
    ("<init>", "l")  >? "o_<init>"

    ("s_h1", "h1") >? "o_h1"
    ("s_h1", "h2") >? "o_<init>"
    ("s_h1", "d1") >? "o_h1"
    ("s_h1", "d2") >? "o_<init>"
    ("s_h1", "l")  >? "o_<init>"

    ("s_h2", "h1") >? "o_<init>"
    ("s_h2", "h2") >? "o_h2"
    ("s_h2", "d1") >? "o_<init>"
    ("s_h2", "d2") >? "o_h2"
    ("s_h2", "l")  >? "o_<init>"

    ("s_h1h2", "h1") >? "o_h1"
    ("s_h1h2", "h2") >? "o_h2"
    ("s_h1h2", "d1") >? "o_h1"
    ("s_h1h2", "d2") >? "o_h2"
    ("s_h1h2", "l")  >? "o_<init>"

    ("s_h2h1", "h1") >? "o_h1"
    ("s_h2h1", "h2") >? "o_h2"
    ("s_h2h1", "d1") >? "o_h1"
    ("s_h2h1", "d2") >? "o_h2"
    ("s_h2h1", "l")  >? "o_<init>"

    ("s_h1h2d1", "h1") >? "o_h1"
    ("s_h1h2d1", "h2") >? "o_h2"
    ("s_h1h2d1", "d1") >? "o_h1d1"
    ("s_h1h2d1", "d2") >? "o_h2"
    ("s_h1h2d1", "l")  >? "o_h1d1"

    ("s_h2h1d1", "h1") >? "o_h1"
    ("s_h2h1d1", "h2") >? "o_h2"
    ("s_h2h1d1", "d1") >? "o_h1d1"
    ("s_h2h1d1", "d2") >? "o_<init>"
    ("s_h2h1d1", "l")  >? "o_h1d1"

    ("s_h1h2d1d2", "h1") >? "o_h1"
    ("s_h1h2d1d2", "h2") >? "o_h2"
    ("s_h1h2d1d2", "d1") >? "o_h1d1"
    ("s_h1h2d1d2", "d2") >? "o_h2d2"
    ("s_h1h2d1d2", "l")  >? "o_h1h2d1d2"

    ("s_h2h1d1d2", "h1") >? "o_h1"
    ("s_h2h1d1d2", "h2") >? "o_h2"
    ("s_h2h1d1d2", "d1") >? "o_h1d1"
    ("s_h2h1d1d2", "d2") >? "o_h2d2"
    ("s_h2h1d1d2", "l")  >? "o_h2h1d1d2"

taSystem = makeSystem ta_policy ta_system

{-
prop_ta_secure :: (GenSingleton d) => System s a d -> [[Action a]] -> Bool
prop_ta_secure sys list =
    let obs_f = SecurityBasics.obs sys in
    List.all (\d ->
        let ta_equivs = split_equiv (ta_test sys d) list in
        List.all (\equiv_class ->
            -- For this purge_equivalence class, generate observation equivalences
            let obs_equivs = split_equiv (\as1 as2 -> let state1 = doRun sys as1 in
                                                      let state2 = doRun sys as2 in
                                                      obs_f (fromJust state1) d == obs_f (fromJust state2) d
                                         ) equiv_class in
            -- If this is TA-secure then we should get [obs-equiv] == TA-equiv.
            (List.head obs_equivs) == equiv_class
        ) ta_equivs
    ) $ allNS (value :: GenSingleton d => Singleton d)
    where ta_test :: System s a d -> Domain d -> [Action a] -> [Action a] -> Bool
          ta_test sys d as1 as2 = let state1 = doRun sys as1 in
                                  let state2 = doRun sys as2 in
                                  if (isJust state1 && isJust state2) then
                                      ta sys as1 d == ta sys as2 d
                                  else
                                      True -- TODO: this shouldn't be true. If we get this it means system isn't input-enabled.
-}

prop_ta_then_ip_specific :: (GenSingleton s, GenSingleton a, GenSingleton d) => System s a d -> Property
prop_ta_then_ip_specific system =
    forAll (getCases system) $ (\list -> prop_ta_secure system list ==> prop_ipurge_secure system list)

ta_ip_check = case taSystem of
    ExES (ES system _) -> quickCheck $ prop_ta_then_ip_specific system

prop_ip_then_ta_specific :: (GenSingleton s, GenSingleton a, GenSingleton d) => System s a d -> Property
prop_ip_then_ta_specific system =
    forAll (getCases system) $ (\list -> prop_ipurge_secure system list ==> prop_ta_secure system list)

ip_ta_check = case taSystem of
    ExES (ES system _) -> quickCheck $ prop_ip_then_ta_specific system

prop_ip_then_ta :: ExistsASSystem -> Property
prop_ip_then_ta (ExAS sys) =
    let system = getASBase sys in
    forAll (getCases system) $ (\list -> prop_ipurge_secure system list ==> prop_ta_secure system list)

args :: Args
args = Args {
    replay     = Nothing,
    maxSuccess = 100,
    maxDiscard = 100,
    maxSize    = 10,
    chatty     = True
    }

