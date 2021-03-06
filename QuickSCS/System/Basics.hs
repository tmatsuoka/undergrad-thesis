{-# LANGUAGE GADTs #-}

module QuickSCS.System.Basics where

import Data.List
import QuickSCS.TypeNats

type State  s = NatSet s
type Action a = NatSet a
type Domain d = NatSet d
type ObservationSymbol = String

data Policy d = Policy { inter :: Domain d -> Domain d -> Bool }
noninter (Policy f) a b = not (f a b)

-- System is also (semi-)parametrised by the domains (and thus policies) too.

data System s a d = System
    { initial :: State  s,
      step    :: State  s -> Action a -> [State s],
      obs     :: State  s -> Domain d -> ObservationSymbol,
      dom     :: Action a -> Domain d,
      -- Ugly stuff below
      policy  :: Policy d,
      actions :: [Action a],
      show_state  :: State  s -> String,
      show_action :: Action a -> String,
      show_domain :: Domain d -> String
    };

data ExistsSystem where
    ExS :: (GenSingleton s, GenSingleton a, GenSingleton d) => System s a d -> ExistsSystem

run :: System s a d -> [State s] -> [Action a] -> [State s]
run _   ss [] = ss
run sys ss (a:as) =
    let nexts = concatMap (\s -> (step sys) s a) ss in
    nub $ concatMap (\x -> run sys [x] as) nexts

doRun :: System s a d -> [Action a] -> [State s]
doRun sys as = run sys [initial sys] as

