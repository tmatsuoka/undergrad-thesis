{-
The implementation of System eDSL (SSDL-Lite).
Parses action declaration and transition function, and stores into corresponding
state list, action list and transition map, with a help of State monad.
-}

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

import Singletons

-- EDSL data type and modification functions

type StateId = String
type ActionId = String
type DomainId = String
type ObservationSymbol = String

-- States, Domains, Domains * Actions, Transition map, Observation map and Interference set
type EDSLData = (Set.Set StateId, Set.Set DomainId, Set.Set (DomainId, ActionId), Map.Map (StateId, ActionId) StateId,
                 Map.Map (StateId, DomainId) ObservationSymbol, Set.Set (DomainId, DomainId))

emptyEDSL :: EDSLData
emptyEDSL = (Set.empty, Set.empty, Set.empty, Map.empty, Map.empty, Set.empty)

addState :: StateId -> EDSLData -> EDSLData
addState state (ss, ds, as, trans, obser, inter) = (Set.insert state ss, ds, as, trans, obser, inter)

addDomain :: DomainId -> EDSLData -> EDSLData
addDomain domain (ss, ds, as, trans, obser, inter) = (ss, Set.insert domain ds, as, trans, obser, inter)

addAction :: DomainId -> ActionId -> EDSLData -> EDSLData
addAction domain action (ss, ds, as, trans, obser, inter) = (ss, ds, Set.insert (domain, action) as, trans, obser, inter)

addActions :: DomainId -> [ActionId] -> EDSLData -> EDSLData
addActions _ [] edsl = edsl
addActions d (a:as) edsl = addActions d as $ addAction d a edsl

addObs :: (StateId, DomainId) -> ObservationSymbol -> EDSLData -> EDSLData
addObs tuple obs (ss, ds, as, trans, obser, inter) = (ss, ds, as, trans, Map.insert tuple obs obser, inter)

addTrans :: (StateId, ActionId) -> StateId -> EDSLData -> EDSLData
addTrans fromTuple to (ss, ds, as, trans, obser, inter) = (ss, ds, as, Map.insert fromTuple to trans, obser, inter)

addInterference :: (DomainId, DomainId) -> EDSLData -> EDSLData
addInterference tuple (ss, ds, as, trans, obser, inter) = (ss, ds, as, trans, obser, Set.insert tuple inter)

-- actions: Declares available actions in the system, and binds them with corresponding security domain.

-- actions :: MonadState EDSLData m => DomainId -> [ActionId] -> m () 
actions domain actions = modify (\edsl -> addActions domain actions $ addDomain domain edsl)

-- obs: Defines observation for each state * domain.

-- obs :: MonadState EDSLData m => (StateId, DomainId) -> ObservationSymbol -> m ()
obs tuple observation = modify $ addObs tuple observation

-- (~>): Transition operator

-- (~>) :: MonadState EDSLData m => (StateId, ActionId) -> StateId -> m ()
(~>) (from, action) to = modify (\edsl -> addTrans (from, action) to $ addState to $ addState from edsl)

-- (>->): Interference operator

-- (>->) :: MonadState EDSLData m => DomainId -> DomainId -> m ()
(>->) a b = modify $ addInterference (a, b)

-- parse: Parses EDSL to build EDSLData

parse :: State (EDSLData) () -> EDSLData
parse desc = execState desc emptyEDSL

-- Get a size of set and wrap with Exists Singleton

sizeSingleton :: Set.Set a -> Exists Singleton
sizeSingleton sets = intToSingleton $ Set.size sets

-- Converts Exists Singleton to equivalent Exists NatSet list

makeNS :: Exists Singleton -> [Exists NatSet]
makeNS (ExistsNat n) = map ExistsOnly (allNS n)

-- Stage 1: Convert states and domain into Singletons.

stage_one (ss, ds, as, trans, obser, inter) = (ss_ns, ds_ns, as, trans, obser, inter)
    where ss_ns = zipWith (\a b -> (a, b)) (makeNS $ sizeSingleton ss) (Set.toList ss)
          ds_ns = zipWith (\a b -> (a, b)) (makeNS $ sizeSingleton ds) (Set.toList ds)


