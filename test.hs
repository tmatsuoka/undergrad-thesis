import QuickSCS.System.Description
import QuickSCS.System.GraphVizDrawer
import QuickSCS.Security.Prelude
import QuickSCS.Checker

arbitrary_policy = do
    domains ["d0","d1","d2","d3","d4"]

    "d0" >-> "d0"
    "d0" >-> "d4"
    "d1" >-> "d1"
    "d1" >-> "d4"
    "d2" >-> "d0"
    "d2" >-> "d2"
    "d3" >-> "d1"
    "d3" >-> "d3"
    "d4" >-> "d4"

arbitrary_system = do
    start "s0"

    actions "d0" ["a2"]
    actions "d1" ["a3"]
    actions "d2" ["a4"]
    actions "d3" ["a0"]
    actions "d4" ["a1"]

    ("s0", "a0") ~> ["s1"]
    ("s0", "a1") ~> ["s1"]
    ("s0", "a2") ~> ["s1"]
    ("s0", "a3") ~> ["s1"]
    ("s0", "a4") ~> ["s1"]
    ("s1", "a0") ~> ["s1"]
    ("s1", "a1") ~> ["s1"]
    ("s1", "a2") ~> ["s1"]
    ("s1", "a3") ~> ["s1"]
    ("s1", "a4") ~> ["s1"]

    ("s0", "d0") >? "5"
    ("s0", "d1") >? "7"
    ("s0", "d2") >? "9"
    ("s0", "d3") >? "2"
    ("s0", "d4") >? "9"
    ("s1", "d0") >? "7"
    ("s1", "d1") >? "3"
    ("s1", "d2") >? "5"
    ("s1", "d3") >? "5"
    ("s1", "d4") >? "5"

asSystem = makeSystem arbitrary_policy arbitrary_system

main = putStr $ showEDSLGraphViz asSystem
-- main = verify asSystem prop_ta_secure
