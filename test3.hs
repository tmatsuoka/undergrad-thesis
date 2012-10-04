import SSDLLite
import DefaultSecurityDefinitions
import SecurityChecker

arbitrary_policy = do
    domains ["d0","d1","d2"]

    "d1" >-> "d0"
    "d0" >-> "d0"

arbitrary_system = do
    SSDLLite.init "s3"

    actions "d0" ["a3"]
    actions "d1" ["a0","a1"]
    actions "d2" ["a2"]

    ("s0", "a1") ~> ["s0"]
    ("s0", "a2") ~> ["s3"]
    ("s0", "a3") ~> ["s1"]
    ("s1", "a1") ~> ["s0"]
    ("s1", "a2") ~> ["s2"]
    ("s1", "a3") ~> ["s0"]
    ("s3", "a1") ~> ["s1"]
    ("s3", "a2") ~> ["s1"]
    ("s3", "a3") ~> ["s2"]

    ("s0", "d0") >? "6"
    ("s0", "d1") >? "1"
    ("s0", "d2") >? "7"
    ("s1", "d0") >? "1"
    ("s1", "d1") >? "1"
    ("s1", "d2") >? "5"
    ("s2", "d0") >? "6"
    ("s2", "d1") >? "2"
    ("s2", "d2") >? "1"
    ("s3", "d0") >? "4"
    ("s3", "d1") >? "3"
    ("s3", "d2") >? "3"

asSystem = makeSystem arbitrary_policy arbitrary_system
