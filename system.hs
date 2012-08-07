-- Rushby's finite system (essentially DFA/NFA)

module System where

data State = State { id_s :: Int } deriving (Show, Eq)

data Action = Action { id_a :: Int } deriving (Show, Eq)

newtype Output = Output Char deriving (Show, Eq)

data System = System { states :: [State],
                       actions :: [Action],
                       initial :: State,
                       step :: State -> Action -> State,
                       output :: State -> Action -> Output
                     }

run :: System -> State -> [Action] -> State
run _ s [] = s
run sys@(System _ _ _ step _) s (a:as) = run sys (step s a) as

s1 = State 1
s2 = State 2
s3 = State 3
a1 = Action 1
a2 = Action 2

sys = System [s1, s2, s3] [a1, a2] s1 ex_sys_step ex_sys_output
    where ex_sys_step s a | s == s1 && a == a1 = s2
                          | s == s1 && a == a2 = s3
                          | s == s2 = s3
                          | s == s3 && a == a1 = s3
                          | s == s3 && a == a2 = s1
          ex_sys_output s a | s == s1 && a == a1 = Output 'h'
                            | s == s1 && a == a2 = Output 'e'
                            | s == s2 = Output 'l'
                            | s == s3 && a == a1 = Output 'l'
                            | s == s3 && a == a2 = Output 'o'

