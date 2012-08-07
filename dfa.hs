data DFA = DFA { states :: [State],
                 start :: State,
                 transitions :: [Transition] }

data State = Intermediate { id :: Int }
           | Accepting { id :: Int } deriving (Show, Eq)

data Transition = Transition { from :: State,
                               input :: Char,
                               to :: State }

next :: [Transition] -> State -> Char -> Maybe State
next ((Transition f i t):ts) origin c = if ((f == origin) && (i == c)) then Just t else next ts origin c
next [] _ _ = Nothing

traverse_rec :: DFA -> State -> [Char] -> [State] -> [State]
traverse_rec dfa@(DFA _ _ ts) s@(Intermediate _) (c:cs) ss = 
    let next_s = next ts s c in
    case next_s of
        Nothing -> ss
        Just ns -> traverse_rec dfa ns cs (ss ++ [ns])
traverse_rec _ (Accepting _) _ ss = ss

traverse :: DFA -> [Char] -> [State]
traverse dfa@(DFA _ start _) cs = traverse_rec dfa start cs [start]

main :: IO ()
main = putStrLn (show (traverse ex_dfa "111"))
    where s1 = Intermediate 1
          s2 = Intermediate 2
          s3 = Intermediate 3
          s4 = Accepting 4
          t1 = Transition s1 '1' s2
          t2 = Transition s2 '1' s3
          t3 = Transition s3 '1' s4
          ex_dfa = DFA [s1, s2, s3, s4] s1 [t1, t2, t3]
