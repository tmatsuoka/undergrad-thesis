{-

The implementation of System eDSL (SSDL-Lite).
Parses action declaration and transition function, and stores into corresponding
state list, action list and transition map, with a help of State monad.

-}

module SSDLLite where

import Control.Monad.State
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Singletons
import qualified SecurityBasics as SB

-- EDSL data type and modification functions

type Id = String
type StateId = Id
type ActionId = Id
type DomainId = Id
type ObservationSymbol = String

type StateIds = Set.Set StateId
type DomainIds = Set.Set DomainId
type DomIds = Set.Set (DomainId, ActionId)
type TransitionIds = Map.Map (StateId, ActionId) StateId
type ObservationIds = Map.Map (StateId, DomainId) ObservationSymbol

-- Interference data

type PrePolicy = (Set.Set DomainId, Set.Set (DomainId, DomainId))

emptyPolicy :: PrePolicy
emptyPolicy = (Set.empty, Set.empty)

setDomains :: [DomainId] -> PrePolicy -> PrePolicy
setDomains ds (_, inter) = (Set.fromList ds, inter)

addInterference :: (DomainId, DomainId) -> PrePolicy -> PrePolicy
addInterference tuple (ds, inter) = (ds, Set.insert tuple inter)

-- domains: Domain definition

-- domains :: MonadState InterferenceIds m => [DomainId] -> m ()
domains ds = modify $ setDomains ds

-- (>->): Interference operator

-- (>->) :: MonadState InterferenceIds m => DomainId -> DomainId -> m ()
(>->) a b = modify $ addInterference (a, b)

-- parsePolicy: Parses interference eDSL to build PrePolicy

parsePolicy :: State (PrePolicy) () -> PrePolicy
parsePolicy desc = execState desc emptyPolicy

-- States, Domains, Domains * Actions, Transition map, Observation map and initial state
type EDSLData = (StateIds, DomIds, TransitionIds, ObservationIds, Maybe StateId)

emptyEDSL :: EDSLData
emptyEDSL = (Set.empty, Set.empty, Map.empty, Map.empty, Nothing)

isStateNull :: EDSLData -> Bool
isStateNull (ss, _, _, _, _) = Set.null ss

addState :: StateId -> EDSLData -> EDSLData
addState state (ss, as, trans, obser, init) = (Set.insert state ss, as, trans, obser, init)

addAction :: DomainId -> ActionId -> EDSLData -> EDSLData
addAction domain action (ss, as, trans, obser, init) = (ss, Set.insert (domain, action) as, trans, obser, init)

addActions :: DomainId -> [ActionId] -> EDSLData -> EDSLData
addActions _ [] edsl = edsl
addActions d (a:as) edsl = addActions d as $ addAction d a edsl

addObs :: (StateId, DomainId) -> ObservationSymbol -> EDSLData -> EDSLData
addObs tuple obs (ss, as, trans, obser, init) = (ss, as, trans, Map.insert tuple obs obser, init)

addTrans :: (StateId, ActionId) -> StateId -> EDSLData -> EDSLData
addTrans fromTuple to (ss, as, trans, obser, init) = (ss, as, Map.insert fromTuple to trans, obser, init)

addInitial :: StateId -> EDSLData -> EDSLData
addInitial initial edsl@(ss, as, trans, obser, init) = case init of 
    Just init_s -> edsl
    Nothing     -> (ss, as, trans, obser, Just initial)

-- actions: Declares available actions in the system, and binds them with corresponding security domain.

-- actions :: MonadState EDSLData m => DomainId -> [ActionId] -> m () 
actions domain actions = modify $ addActions domain actions

-- obs: Defines observation for each state * domain.

-- obs :: MonadState EDSLData m => (StateId, DomainId) -> ObservationSymbol -> m ()
obs tuple observation = modify $ addObs tuple observation

-- init: Defines initial state.

-- init :: MonadState EDSLData m => StateId -> m ()
init state = modify $ addInitial state

-- (~>): Transition operator

-- (~>) :: MonadState EDSLData m => (StateId, ActionId) -> StateId -> m ()
(~>) (from, action) to = modify (\edsl -> addTrans (from, action) to $ addState to $ addState from $ addInitial from edsl)

-- parse: Parses eDSL to build EDSLData

parse :: State (EDSLData) () -> EDSLData
parse desc = execState desc emptyEDSL

-- Types for intermediate semi-system representation, and for the final system

type EDSLThing = Exists NatSet
type EDSLState = Exists NatSet
type EDSLDomain = Exists NatSet
type EDSLAction = Exists NatSet

type Assoc = [(EDSLThing, Id)]
type StateAssoc = [(EDSLState, StateId)]
type DomainAssoc = [(EDSLDomain, DomainId)]
type ActionAssoc = [(EDSLAction, ActionId)]
type DomIntermediate = [(EDSLDomain, EDSLAction)]
type InterferenceAssoc = [(EDSLDomain, EDSLDomain)]

type PolicyIntermediate = (DomainAssoc, InterferenceAssoc)
type EDSLIntermediate = (StateAssoc, DomainAssoc, ActionAssoc, DomIntermediate, TransitionIds, ObservationIds, InterferenceAssoc, EDSLState)

-- Get a size of set and wrap with Exists Singleton

sizeSingleton :: Set.Set a -> Exists Singleton
sizeSingleton sets = intToSingleton $ Set.size sets

-- Converts Exists Singleton to equivalent Exists NatSet list

makeNS :: Exists Singleton -> [Exists NatSet]
makeNS (ExistsNat n) = map ExistsOnly (allNS n)

-- Converts a set of states/domains/action identifiers to equivalent associative list

convertNS :: Set.Set Id -> Assoc
convertNS set = zipWith (\a b -> (a, b)) (makeNS $ sizeSingleton set) (Set.toList set)

-- Finds an equivalent Exists NatSet from the associative list.
-- Aborts the program when element isn't found, but for eDSL parsing we already know it should be there.

lookupNS :: Id -> Assoc -> EDSLThing
lookupNS name list = case List.find (\(ns, id) -> id == name) list of
    Just (ns, id) -> ns
    Nothing       -> error ("NatSet for identifier " ++ name ++ " was not found.")

-- This is why we need Eq for Exists NatSet.

lookupId :: EDSLThing -> Assoc -> Id
lookupId thing list = case List.find (\(ns, id) -> ns == thing) list of
    Just (ns, id) -> id
    Nothing       -> error ("Identifier for NatSet" ++ show thing ++ " was not found.")

-- Stage 1: Convert parsed EDSL data into intermediate representation (Exists NatSet).

prepare :: PrePolicy -> EDSLData -> EDSLIntermediate
prepare (ds, inter) (ss, as, trans, obser, init) = (ss_ns, ds_ns, as_ns, dom, trans, obser, inter_ns, init_ns)
    where ss_ns = convertNS ss
          ds_ns = convertNS ds
          as_ns = convertNS $ Set.map (\tuple -> snd tuple) as
          dom = map (\(d, a) -> (lookupNS d ds_ns, lookupNS a as_ns)) (Set.toList as)
          inter_ns = map (\(d1, d2) -> (lookupNS d1 ds_ns, lookupNS d2 ds_ns)) $ Set.toList inter
          init_ns = case init of
            Just init_s -> lookupNS init_s ss_ns
            Nothing     -> error ("No initial state detected")
          -- We aren't converting transition/observation table here, as it's inefficient and
          -- Map.map/mapKeys can't handle Exists NatSet (which isn't a member of Ord)

-- step function we're using for eDSL-built systems. Notice we are passing states/actions/transitions here.

eDSLstep :: StateAssoc -> ActionAssoc -> TransitionIds -> EDSLState -> EDSLAction -> EDSLState
eDSLstep ss as trans from action = next
    where s_id    = lookupId from ss
          a_id    = lookupId action as
          next_id = trans Map.! (s_id, a_id)
          next    = lookupNS next_id ss

eDSLobs :: StateAssoc -> DomainAssoc -> ObservationIds -> EDSLState -> EDSLDomain -> ObservationSymbol
eDSLobs ss ds obs s d = obser
    where s_id  = lookupId s ss
          d_id  = lookupId d ds
          obser = obs Map.! (s_id, d_id)

eDSLdom :: DomIntermediate -> EDSLAction -> EDSLDomain
eDSLdom dom action = case List.find (\(d, a) -> a == action) dom of
    Just (d, a) -> a
    Nothing     -> error ("EDSL Dom: domain for action " ++ show action ++ " was not found.")

-- Generic interference function for EDSL-defined security policy

edslPolicyInter :: InterferenceAssoc -> EDSLDomain -> EDSLDomain -> Bool
edslPolicyInter inter_ns a b = List.elem (a, b) inter_ns

-- Builds a Policy from EDSL-defined interferences

buildPolicy :: InterferenceAssoc -> SB.Policy EDSLDomain
buildPolicy inter_ns = SB.Policy { SB.inter = edslPolicyInter inter_ns }

-- Stage 2: Pack the intermediate representation into System.

build (ss_ns, ds_ns, as_ns, dom, trans, obser, inter_ns, init_ns) = SB.System {
    SB.initial     = init_ns,
    -- SB.initial     = fst $ head ss_ns, -- For now. Obviously we need a way to specify this in eDSL.

    -- Functions below need states/domains/actions associative list and transition tables.
    -- But System doesn't have to store them - Haskell has partially applied function!
    SB.step        = eDSLstep ss_ns as_ns trans,
    SB.obs         = eDSLobs ss_ns ds_ns obser,
    SB.dom         = eDSLdom dom,
    SB.action_list = map (\x -> fst x) as_ns,
    SB.policy      = buildPolicy inter_ns
}

-- A single do-it-all function that takes in EDSL description for policy and system and builds SB.System (which includes SB.Policy)

makeSystem policyDesc systemDesc = build $ prepare (parsePolicy policyDesc) (parse systemDesc)

