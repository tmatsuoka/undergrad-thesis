import Test.QuickCheck
import SecurityChecker

args :: Args
args = Args {
    replay     = Nothing,
    maxSuccess = 100,
    maxDiscard = 100,
    maxSize    = 7,
    chatty     = True
    }

main = quickCheckWith args prop_ip_and_p


