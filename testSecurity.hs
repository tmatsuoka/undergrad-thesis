import Test.QuickCheck
import SecurityChecker

args :: Args
args = Args {
    replay     = Nothing,
    maxSuccess = 250,
    maxDiscard = 250,
    maxSize    = 10,
    chatty     = True
    }

main = quickCheckWith args prop_ip_then_p

