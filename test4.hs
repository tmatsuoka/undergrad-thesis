import Singletons
import SecurityBasics
import SSDLLite
import SecurityChecker
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
    ("s_h2h1"  , "a_d2") ~> ["s_h2h1d2"]
    ("s_h1h2d1", "a_d2") ~> ["s_h1h2d1d2"]
    ("s_h2h1d2", "a_d1") ~> ["s_h2h1d2d1"]

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

    ("s_h2h1d2", "h1") >? "o_h1"
    ("s_h2h1d2", "h2") >? "o_h2"
    ("s_h2h1d2", "d1") >? "o_h1"
    ("s_h2h1d2", "d2") >? "o_h2d2"
    ("s_h2h1d2", "l")  >? "o_h2d2"

    ("s_h1h2d1d2", "h1") >? "o_h1"
    ("s_h1h2d1d2", "h2") >? "o_h2"
    ("s_h1h2d1d2", "d1") >? "o_h1d1"
    ("s_h1h2d1d2", "d2") >? "o_h2d2"
    ("s_h1h2d1d2", "l")  >? "o_h1h2d1d2"

    ("s_h2h1d2d1", "h1") >? "o_h1"
    ("s_h2h1d2d1", "h2") >? "o_h2"
    ("s_h2h1d2d1", "d1") >? "o_h1d1"
    ("s_h2h1d2d1", "d2") >? "o_h2d2"
    ("s_h2h1d2d1", "l")  >? "o_h2h1d2d1"

taSystem = makeSystem ta_policy ta_system

prop_p_then_ip_specific :: (GenSingleton s, GenSingleton a, GenSingleton d) => System s a d -> Property
prop_p_then_ip_specific system =
    forAll (do
        list1a <- getCases system
        list1b <- getCases system
        list2a <- getCases system
        list2b <- getCases system
        return (list1a, list1b, list2a, list2b)
    ) $ (\(list1a, list1b, list2a, list2b) -> prop_purge_secure system list1a list1b ==> prop_ipurge_secure system list2a list2b)

prop_ip_then_p_specific :: (GenSingleton s, GenSingleton a, GenSingleton d) => System s a d -> Property
prop_ip_then_p_specific system =
    forAll (do
        list1a <- getCases system
        list1b <- getCases system
        list2a <- getCases system
        list2b <- getCases system
        return (list1a, list1b, list2a, list2b)
    ) $ (\(list1a, list1b, list2a, list2b) -> prop_ipurge_secure system list1a list1b ==> prop_purge_secure system list2a list2b)

p_ip_check = case taSystem of
    ExES (ES system _) -> verboseCheck $ prop_p_then_ip_specific system

ip_p_check = case taSystem of
    ExES (ES system _) -> verboseCheck $ prop_ip_then_p_specific system
