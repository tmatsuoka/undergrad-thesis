import SSDLLite

my_policy = do
    domains ["low", "high"]
    "low" >-> "high"

my_sys = do
    actions "low" ["A1", "A2"]
    ("S1", "A1") ~> ["S2"]
    ("S1", "A2") ~> ["S3"]
    ("S2", "A1") ~> ["S3"]
    -- ("S2", "A2") ~> ["S3"]
    -- ("S3", "A1") ~> ["S3"]
    ("S3", "A2") ~> ["S1"]
    obs ("S1", "low") "O1"
    obs ("S1", "high") "O1"
    obs ("S2", "low") "O2"
    obs ("S2", "high") "O2"
    obs ("S3", "low") "O3"
    obs ("S3", "high") "O3"

mySystem = makeSystem my_policy my_sys

main = print mySystem

