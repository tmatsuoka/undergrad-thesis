import Test.QuickCheck
import QuickSCS.System.Description
import QuickSCS.Security.Prelude
import QuickSCS.Checker

{-
ip_policy = do
    domains ["H", "D", "L"]
    "H" >-> "D"
    "D" >-> "L"
-}

ta_policy = do
    domains ["H1", "H2", "D1", "D2", "L"]
    "H1" >-> "D1"
    "H2" >-> "D2"
    "D1" >-> "L"
    "D2" >-> "L"

main = separate prop_ipurge_secure prop_ta_secure (Just $ makePolicy ta_policy)

