import ICS.System.Description
import ICS.Security.Prelude
import ICS.Checker

arbitrary_policy = do
    domains ["d0","d1","d2"]

    "d0" >-> "d1"
    "d1" >-> "d2"

arbitrary_system = do
    start "s0"

    actions "d0" ["a0"]
    actions "d1" ["a1"]
    actions "d2" ["a2"]

    ("s0", "a0") ~> ["s0"]
    ("s0", "a1") ~> ["s2"]
    ("s0", "a2") ~> ["s1"]
    ("s1", "a0") ~> ["s0"]
    ("s1", "a1") ~> ["s2"]
    ("s1", "a2") ~> ["s0"]
    ("s2", "a0") ~> ["s1"]
    ("s2", "a1") ~> ["s1"]
    ("s2", "a2") ~> ["s2"]

    ("s0", "d0") >? "6"
    ("s0", "d1") >? "1"
    ("s0", "d2") >? "7"
    ("s1", "d0") >? "1"
    ("s1", "d1") >? "1"
    ("s1", "d2") >? "5"
    ("s2", "d0") >? "6"
    ("s2", "d1") >? "2"
    ("s2", "d2") >? "1"

asSystem = makeSystem arbitrary_policy arbitrary_system
