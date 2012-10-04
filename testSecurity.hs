import Test.QuickCheck
import SecurityChecker

args :: Args
args = Args {
    replay     = Nothing,
    maxSuccess = 500,
    maxDiscard = 100,
    maxSize    = 10,
    chatty     = True
    }

main = quickCheckWith args prop_p_then_ip

