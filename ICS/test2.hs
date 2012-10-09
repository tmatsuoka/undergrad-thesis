{-# LANGUAGE GADTs, DataKinds #-}

-- Test system from Ron van der Meyden's prelim report, "What, indeed, is non-transitive interference?"

module Main where

import SecurityBasics
import ThreeLevelMLS
import SecuritySystem
import DefaultSecurityDefinitions

data MyState = S1 | S2 | S3
instance Show MyState where
    show S1 = "S1"
    show S2 = "S2"
    show S3 = "S3"

-- data MyAction = A1 | A2
data MyObservation = O1 | O2 | O3
instance Show MyObservation where
    show O1 = "O1"
    show O2 = "O2"
    show O3 = "O3"

my_sys_step :: MyState -> NatSet n -> MyState
my_sys_step S1 (NSSuc NSZero) = S2
my_sys_step S1 (NSSuc (NSSuc NSZero)) = S3
my_sys_step S2 _  = S3
my_sys_step S3 (NSSuc NSZero) = S3
my_sys_step S3 (NSSuc (NSSuc NSZero)) = S1

my_sys_obs :: MyState -> LowHigh -> MyObservation
my_sys_obs S1 Low  = O1
my_sys_obs S1 High = O1
my_sys_obs S2 Low  = O2
my_sys_obs S2 High = O2
my_sys_obs S3 Low  = O3
my_sys_obs S3 High = O3

my_sys_dom :: NatSet n -> LowHigh
my_sys_dom (NSSuc NSZero) = Low
my_sys_dom (NSSuc (NSSuc NSZero)) = Low

my_sys :: System MyState (NatSet (Suc (Suc Zero))) MyObservation LowHigh
my_sys = System {
        initial = S1,
        step = my_sys_step,
        obs = my_sys_obs,
        dom = my_sys_dom,
        action_list = [NSZero, (NSSuc NSZero), (NSSuc (NSSuc NSZero))]
    }
{-
a1 = Action 1
a2 = Action 2

sys = System [s1, s2, s3] [a1, a2] s1 ex_sys_step ex_sys_output ex_sys_dom
    where s1 = State 1
          s2 = State 2
          s3 = State 3
          ex_sys_step s a | s == s1 && a == a1 = s2
                          | s == s1 && a == a2 = s3
                          | s == s2 = s3
                          | s == s3 && a == a1 = s3
                          | s == s3 && a == a2 = s1
          ex_sys_output s a | s == s1 && a == a1 = Output 'h'
                            | s == s1 && a == a2 = Output 'e'
                            | s == s2 = Output 'l'
                            | s == s3 && a == a1 = Output 'l'
                            | s == s3 && a == a2 = Output 'o'
          ex_sys_dom a | a == a1 = Low
                       | a == a2 = Low
-}

main = print "Hello World!"
