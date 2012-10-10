import Test.QuickCheck
import ICS.Checker

args :: Args
args = Args {
    replay     = Nothing,
    maxSuccess = 100,
    maxDiscard = 100,
    maxSize    = 10,
    chatty     = True
    }

main = quickCheckWith args prop_ip_then_p

