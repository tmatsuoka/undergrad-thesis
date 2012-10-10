module TwoLevelMLS where

import SecurityBasics

data LowHigh = Low | High
instance Eq LowHigh where
    (==) Low Low = True
    (==) High High = True
    (==) _ _ = False

instance Show LowHigh where
    show Low = "Low"
    show High = "High"

lowHighPolicy :: Policy LowHigh
lowHighPolicy = Policy {
    inter = lowHighPolicyInter
} where
    lowHighPolicyInter Low High = False
    lowHighPolicyInter _   _    = True

{-
instance Policy LowHigh where
    inter Low High = False
    inter _   _    = True
-}
