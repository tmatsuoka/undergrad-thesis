module Main where

import SecurityBasics
import TwoLevelMLS

data MyState = S1 | S2 | S3
data MyAction = A1 | A2
data MyObservation = O1 | O2 | O3

my_sys_step :: MyState -> MyAction -> MyState
my_sys_step S1 A1 = S2
my_sys_step S1 A2 = S3
my_sys_step S2 _  = S3
my_sys_step S3 A1 = S3
my_sys_step S3 A2 = S1

my_sys_obs :: MyState -> LowHigh -> MyObservation
my_sys_obs S1 Low  = O1
my_sys_obs S1 High = O1
my_sys_obs S2 Low  = O2
my_sys_obs S2 High = O2
my_sys_obs S3 Low  = O3
my_sys_obs S3 High = O3

my_sys_dom :: MyAction -> LowHigh
my_sys_dom A1 = Low
my_sys_dom A2 = Low

my_sys :: System MyState MyAction MyObservation LowHigh
my_sys = System {
        initial = S1,
        step = my_sys_step,
        obs = my_sys_obs,
        dom = my_sys_dom
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
