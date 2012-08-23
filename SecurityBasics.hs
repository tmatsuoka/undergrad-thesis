module SecurityBasics where

newtype Policy a = Policy { inter :: a -> a -> Bool }
noninter (Policy f) a b = not (f a b)

-- System is also (semi-)parametrised by the domains (and thus policies) too.
data System state action obs domain = System
                { initial :: state,
                  step :: state -> action -> state,
                  obs :: state -> domain -> obs,
                  dom :: action -> domain
                };

{-
instance (Arbitrary s, Arbitrary a, Arbitary d) => Arbitrary (System s a d)


data ExistsSystem :: * where
   ExI :: System s a d -> ExistsSystem
-}

