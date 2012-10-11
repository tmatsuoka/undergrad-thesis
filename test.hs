import QuickSCS.System.Description
import QuickSCS.System.GraphVizDrawer

arbitrary_policy = do
    domains ["d0","d1","d2"]

    "d0" >-> "d0"
    "d1" >-> "d1"
    "d2" >-> "d2"
    "d0" >-> "d2"
    "d0" >-> "d1"

arbitrary_system = do
    start "s0"

    actions "d0" ["a0"]
    actions "d1" ["a3"]
    actions "d2" ["a1","a2"]

    ("s0", "a0") ~> ["s0"]
    ("s0", "a1") ~> ["s0"]
    ("s0", "a2") ~> ["s0"]
    ("s0", "a3") ~> ["s3"]
    ("s1", "a0") ~> ["s2"]
    ("s1", "a1") ~> ["s0"]
    ("s1", "a2") ~> ["s0"]
    ("s1", "a3") ~> ["s0"]
    ("s2", "a0") ~> ["s1"]
    ("s2", "a1") ~> ["s2"]
    ("s2", "a2") ~> ["s3"]
    ("s2", "a3") ~> ["s1"]
    ("s3", "a0") ~> ["s1"]
    ("s3", "a1") ~> ["s0"]
    ("s3", "a2") ~> ["s0"]
    ("s3", "a3") ~> ["s0"]

    ("s0", "d0") >? "5"
    ("s0", "d1") >? "7"
    ("s0", "d2") >? "7"
    ("s1", "d0") >? "10"
    ("s1", "d1") >? "11"
    ("s1", "d2") >? "10"
    ("s2", "d0") >? "7"
    ("s2", "d1") >? "4"
    ("s2", "d2") >? "7"
    ("s3", "d0") >? "4"
    ("s3", "d1") >? "11"
    ("s3", "d2") >? "6"

asSystem = makeSystem arbitrary_policy arbitrary_system

main = putStr $ showEDSLGraphViz asSystem
