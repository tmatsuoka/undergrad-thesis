module ThreeLevelMLS where

import SecurityBasics

data LowDowngraderHigh = Low | Downgrader | High deriving Eq

instance Show LowDowngraderHigh where
    show Low = "Low"
    show Downgrader = "Downgrader"
    show High = "High"

lowDowngraderHighPolicy :: Policy LowDowngraderHigh
lowDowngraderHighPolicy = Policy {
    inter = lowDowngraderHighPolicyInter
} where
    lowDowngraderHighPolicyInter Low High = False
    lowDowngraderHighPolicyInter _   _    = True

