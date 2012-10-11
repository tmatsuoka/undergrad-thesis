import QuickSCS.System.Description
import QuickSCS.Security.Prelude
import QuickSCS.Checker
import QuickSCS.System.GraphVizDrawer

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

    -- Nagged by EDSL parser
    ("<init>", "a_d1") ~> ["s_bottom"]
    ("<init>", "a_d2") ~> ["s_bottom"]
    ("<init>", "a_l") ~> ["s_bottom"]
    ("s_h1", "a_d1") ~> ["s_bottom"]
    ("s_h1", "a_d2") ~> ["s_bottom"]
    ("s_h1", "a_h1") ~> ["s_bottom"]
    ("s_h1", "a_l") ~> ["s_bottom"]
    ("s_h1h2", "a_d2") ~> ["s_bottom"]
    ("s_h1h2", "a_h1") ~> ["s_bottom"]
    ("s_h1h2", "a_h2") ~> ["s_bottom"]
    ("s_h1h2", "a_l") ~> ["s_bottom"]
    ("s_h1h2d1", "a_d1") ~> ["s_bottom"]
    ("s_h1h2d1", "a_h1") ~> ["s_bottom"]
    ("s_h1h2d1", "a_h2") ~> ["s_bottom"]
    ("s_h1h2d1", "a_l") ~> ["s_bottom"]
    ("s_h1h2d1d2", "a_d1") ~> ["s_bottom"]
    ("s_h1h2d1d2", "a_d2") ~> ["s_bottom"]
    ("s_h1h2d1d2", "a_h1") ~> ["s_bottom"]
    ("s_h1h2d1d2", "a_h2") ~> ["s_bottom"]
    ("s_h1h2d1d2", "a_l") ~> ["s_bottom"]
    ("s_h2", "a_d1") ~> ["s_bottom"]
    ("s_h2", "a_d2") ~> ["s_bottom"]
    ("s_h2", "a_h2") ~> ["s_bottom"]
    ("s_h2", "a_l") ~> ["s_bottom"]
    ("s_h2h1", "a_d2") ~> ["s_bottom"]
    ("s_h2h1", "a_h1") ~> ["s_bottom"]
    ("s_h2h1", "a_h2") ~> ["s_bottom"]
    ("s_h2h1", "a_l") ~> ["s_bottom"]
    ("s_h2h1d1", "a_d1") ~> ["s_bottom"]
    ("s_h2h1d1", "a_h1") ~> ["s_bottom"]
    ("s_h2h1d1", "a_h2") ~> ["s_bottom"]
    ("s_h2h1d1", "a_l") ~> ["s_bottom"]
    ("s_h2h1d1d2", "a_d1") ~> ["s_bottom"]
    ("s_h2h1d1d2", "a_d2") ~> ["s_bottom"]
    ("s_h2h1d1d2", "a_h1") ~> ["s_bottom"]
    ("s_h2h1d1d2", "a_h2") ~> ["s_bottom"]
    ("s_h2h1d1d2", "a_l") ~> ["s_bottom"]
    ("s_bottom", "a_d1") ~> ["s_bottom"]
    ("s_bottom", "a_d2") ~> ["s_bottom"]
    ("s_bottom", "a_h1") ~> ["s_bottom"]
    ("s_bottom", "a_h2") ~> ["s_bottom"]
    ("s_bottom", "a_l") ~> ["s_bottom"]

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

    -- Nagged by EDSL parser
    ("s_bottom", "d1") >? "o_bottom"
    ("s_bottom", "d2") >? "o_bottom"
    ("s_bottom", "h1") >? "o_bottom"
    ("s_bottom", "h2") >? "o_bottom"
    ("s_bottom", "l") >? "o_bottom"

-- taPolicy = makePolicy ta_policy
taSystem = makeSystem ta_policy ta_system

main = putStrLn $ showEDSLGraphViz taSystem
-- main = verify taSystem prop_ta_secure
