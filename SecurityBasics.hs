{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances, ScopedTypeVariables #-}

module SecurityBasics where

import Data.List
import Singletons

type State  s = NatSet s
type Action a = NatSet a
type Domain d = NatSet d
type ObservationSymbol = String

data Policy d = Policy { inter :: Domain d -> Domain d -> Bool }
noninter (Policy f) a b = not (f a b)

-- System is also (semi-)parametrised by the domains (and thus policies) too.

data System s a d = System
    { initial :: State  s,
      step    :: State  s -> Action a -> Maybe (State s),
      obs     :: State  s -> Domain d -> ObservationSymbol,
      dom     :: Action a -> Domain d,
      policy  :: Policy d --temporary?
    };

data ExistsSystem where
    ExS :: (GenSingleton s, GenSingleton a, GenSingleton d) => System s a d -> ExistsSystem

run :: System s a d -> State s -> [Action a] -> Maybe (State s)
run _ s [] = Just s
run sys s (a:as) = (step sys) s a >>= (\x -> run sys x as)

doRun :: System s a d -> [Action a] -> Maybe (State s)
doRun sys as = run sys (initial sys) as

{-
instance (Arbitrary s, Arbitrary a, Arbitary d) => Arbitrary (System s a d)


data ExistsSystem :: * where
   ExI :: System s a d -> ExistsSystem
-}

