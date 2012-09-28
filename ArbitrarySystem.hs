module ArbitrarySystem where

import Test.QuickCheck
import Test.QuickCheck.Gen

import Singletons

instance Arbitrary Nat where
    arbitrary = (arbitrary :: Gen (NonNegative Int)) >>= \(NonNegative x) -> return (intToNat x)

instance Arbitrary (Exists Singleton) where
    arbitrary = natToSingleton $ arbitrary :: Gen Nat

