module TwoLevelMLS where

import SecurityBasics

data LowHigh = Low | High

{-
instance Policy LowHigh where
    inter Low High = False
    inter _   _    = True
-}
