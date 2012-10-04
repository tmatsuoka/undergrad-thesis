{-# LANGUAGE GADTs #-}

{-

The implementation of System eDSL (SSDL-Lite).
Parses action declaration and transition function, and stores into corresponding
state list, action list and transition map, with a help of State monad.

-}

module SSDLLite where
-- module SSDLLite (domains, (>->), actions, obs, (>?), SSDLLite.init, (~>),
--                  makeSystem, stepEDSL, obsEDSL, domEDSL, runEDSL, doRunEDSL, getBase) where

import Control.Monad.State
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Singletons
import qualified SecurityBasics as SB
import SecurityBasics (ObservationSymbol)

-- EDSL data type and modification functions

type Id                = String
type StateId           = Id
type ActionId          = Id
type DomainId          = Id

type StateIds       = Set StateId
type DomainIds      = Set DomainId
type DomIds         = Set (DomainId, ActionId)
type TransitionIds  = Map (StateId, ActionId) [StateId]
type ObservationIds = Map (StateId, DomainId) ObservationSymbol

-- Interference data

type PrePolicy = (Set DomainId, Set (DomainId, DomainId))

emptyPolicy :: PrePolicy
emptyPolicy = (Set.empty, Set.empty)

addDomains :: [DomainId] -> PrePolicy -> PrePolicy
addDomains ds (prev_ds, inter) = (Set.union prev_ds (Set.fromList ds), Set.union inter (Set.fromList $ map (\d -> (d, d)) ds))

addInterference :: (DomainId, DomainId) -> PrePolicy -> PrePolicy
addInterference tuple (ds, inter) = (ds, Set.insert tuple inter)

-- domains: Domain definition

-- domains :: MonadState InterferenceIds m => [DomainId] -> m ()
domains ds = modify $ addDomains ds

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

addStates :: [StateId] -> EDSLData -> EDSLData
addStates [] edsl = edsl
addStates (state:rest) (ss, as, trans, obser, init) = addStates rest (Set.insert state ss, as, trans, obser, init)

addAction :: DomainId -> ActionId -> EDSLData -> EDSLData
addAction domain action (ss, as, trans, obser, init) = (ss, Set.insert (domain, action) as, trans, obser, init)

addActions :: DomainId -> [ActionId] -> EDSLData -> EDSLData
addActions _ [] edsl = edsl
addActions d (a:as) edsl = addActions d as $ addAction d a edsl

addObs :: (StateId, DomainId) -> ObservationSymbol -> EDSLData -> EDSLData
addObs tuple obs (ss, as, trans, obser, init) = (ss, as, trans, Map.insert tuple obs obser, init)

addTrans :: (StateId, ActionId) -> [StateId] -> EDSLData -> EDSLData
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

-- (>?): Observation operator. In-fix version of obs (see above).

-- (>?) :: MonadState EDSLData m => (StateId, DomainId) -> ObservationSymbol -> m ()
(>?) tuple observation = obs tuple observation

-- init: Defines initial state.

-- init :: MonadState EDSLData m => StateId -> m ()
init state = modify $ addInitial state

-- (~>): Transition operator

-- (~>) :: MonadState EDSLData m => (StateId, ActionId) -> [StateId] -> m ()
(~>) (from, action) to = modify (\edsl -> addTrans (from, action) to $ addStates to $ addState from $ addInitial from edsl)

-- parse: Parses eDSL to build EDSLData

parse :: State (EDSLData) () -> EDSLData
parse desc = execState desc emptyEDSL

-- Types for intermediate semi-system representation, and for the final system

type EDSLThing  n = NatSet n
type EDSLState  n = EDSLThing n
type EDSLDomain n = EDSLThing n
type EDSLAction n = EDSLThing n

type Assoc       n = (EDSLThing n, Id)
type StateAssoc  n = (EDSLState n, StateId)
type DomainAssoc n = (EDSLDomain n, DomainId)
type ActionAssoc n = (EDSLAction n, ActionId)

type AList       n = [Assoc n]
type StateAList  n = [StateAssoc n]
type DomainAList n = [DomainAssoc n]
type ActionAList n = [ActionAssoc n]
type DomIntermediate d a = [(EDSLDomain d, EDSLAction a)]
type TransIntermediate s a = Map (EDSLState s, EDSLAction a) [(EDSLState s)]
type ObserIntermediate s d = Map (EDSLState s, EDSLDomain d) ObservationSymbol
type InterferenceAssoc n = [(EDSLDomain n, EDSLDomain n)]

type PolicyIntermediate n = (DomainAssoc n, InterferenceAssoc n)
type EDSLIntermediate s a d =
    (StateAList s, DomainAList d, ActionAList a, DomIntermediate d a,
     TransIntermediate s a, ObserIntermediate s d, InterferenceAssoc d, EDSLState s)

data ExistsAList where
    ExA :: (GenSingleton n) => AList n -> ExistsAList

data ExistsEDSLIntermediate where
    ExI :: (GenSingleton s, GenSingleton a, GenSingleton d) => EDSLIntermediate s a d -> ExistsEDSLIntermediate

applyFirst :: (a -> b) -> (a, c) -> (b, c)
applyFirst f (a, c) = (f a, c)

-- Converts a list of states/domains/action identifiers to equivalent associative list

convertNS :: [Id] -> ExistsAList
convertNS ids = case natToSingleton $ intToNat $ List.length ids of
    ExistsNat sing -> ExA (zipWith (\ns id -> (ns, id)) (allNS sing) ids)

-- Finds an equivalent Exists NatSet from the associative list.
-- Aborts the program when element isn't found, but for eDSL parsing we already know it should be there.

lookupNS :: Id -> AList n -> EDSLThing n
lookupNS name list = case List.find (\(ns, id) -> id == name) list of
    Just (ns, id) -> ns
    Nothing       -> error ("NatSet for identifier " ++ name ++ " was not found.")

-- Reverse of lookupNS.

lookupId :: EDSLThing n -> AList n -> Id
lookupId thing list = case List.find (\(ns, id) -> ns == thing) list of
    Just (ns, id) -> id
    Nothing       -> error ("Identifier for NatSet" ++ show thing ++ " was not found.")

-- Stage 1: Convert parsed EDSL data into intermediate representation (NatSet n).
-- Holy crap this is ugly - anyone please come up with a better way to write this?

genThing :: AList n -> Id -> EDSLThing n
genThing list id = lookupNS id list

genThingTwo :: AList m -> AList n -> (Id, Id) -> (EDSLThing m, EDSLThing n)
genThingTwo list1 list2 (id1, id2) = (genThing list1 id1, genThing list2 id2)

prepare :: PrePolicy -> EDSLData -> ExistsEDSLIntermediate
prepare (ds, inter) (ss, as, trans, obser, init) = 
    case convertNS $ Set.toList ss of
      ExA ss_ns -> case convertNS $ Set.toList ds of
        ExA ds_ns -> case convertNS $ map (\tuple -> snd tuple) $ Set.toList as of
          ExA as_ns -> ExI (ss_ns, ds_ns, as_ns, dom, trans_ns, obser_ns, inter_ns, init_ns)
            where
              dom      = map (genThingTwo ds_ns as_ns) (Set.toList as)
              trans_ns = Map.map (\states -> map (genThing ss_ns) states) $ Map.mapKeys (genThingTwo ss_ns as_ns) trans
              obser_ns = Map.mapKeys (genThingTwo ss_ns ds_ns) obser
              inter_ns = map (genThingTwo ds_ns ds_ns) $ Set.toList inter
              init_ns  = case init of 
                Just init_id -> genThing ss_ns init_id
                Nothing      -> error "No initial state detected"

-- step function we're using for eDSL-built systems. Notice we are passing states/actions/transitions here.

eDSLstep :: TransIntermediate s a -> EDSLState s -> EDSLAction a -> Maybe (EDSLState s)
eDSLstep trans from action =
    if (Map.member (from, action) trans) then
        let nexts = trans Map.! (from, action) in
        if (List.null nexts) then
            Nothing
        else
            (Just (List.head nexts))
    else
         Nothing

eDSLobs :: ObserIntermediate s d -> EDSLState s -> EDSLDomain d -> ObservationSymbol
eDSLobs obs s d = obs Map.! (s, d)

eDSLdom :: DomIntermediate m n -> EDSLAction n -> EDSLDomain m
eDSLdom dom action = case List.find (\(d, a) -> a == action) dom of
    Just (d, a) -> d
    Nothing     -> error ("EDSL Dom: domain for action " ++ show action ++ " was not found.")

-- Generic interference function for EDSL-defined security policy

edslPolicyInter :: InterferenceAssoc n -> EDSLDomain n -> EDSLDomain n -> Bool
edslPolicyInter inter_ns a b = List.elem (a, b) inter_ns

-- Builds a Policy from EDSL-defined interferences

buildPolicy :: InterferenceAssoc d -> SB.Policy d
buildPolicy inter_ns = SB.Policy { SB.inter = edslPolicyInter inter_ns }

data EDSLSystem s a d where 
    ES :: (GenSingleton s, GenSingleton a, GenSingleton d) => SB.System s a d -> EDSLIntermediate s a d -> EDSLSystem s a d

data ExistsEDSLSystem where
    ExES :: (GenSingleton s, GenSingleton a, GenSingleton d) => EDSLSystem s a d -> ExistsEDSLSystem

getBase :: EDSLSystem s a d -> SB.System s a d
getBase (ES base _) = base

getIntermediate :: EDSLSystem s a d -> EDSLIntermediate s a d
getIntermediate (ES _ intermediate) = intermediate

instance Show ExistsEDSLSystem where
    show (ExES sys) = "=== EDSL generated system ===\n" ++
                      "Initial state: " ++ show (SB.initial base_sys) ++ "\n" ++
                      "States: " ++ show ss_ns ++ "\n" ++
                      "Domains: " ++ show ds_ns ++ "\n" ++
                      "Actions: " ++ show dom ++ "\n" ++
                      "Transitions: " ++ show_trans ++ "\n" ++
                      "Observations: " ++ show_obs ++ "\n" ++
                      "Interferences: " ++ show_inter
        where base_sys = getBase sys
              (ss_ns, ds_ns, as_ns, dom, trans_ns, obser, inter_ns, _) = getIntermediate sys
              show_trans = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\((from, action), to) -> "(" ++ show from ++ ", " ++ show action ++ ") ~> " ++ show to) $
                            Map.toList trans_ns)
              show_obs   = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\((s, d), o) -> "(" ++ show s ++ ", " ++ show d ++ ") >? " ++ show o) $
                            Map.toList obser)
              show_inter = foldl (\a b -> a ++ "\n" ++ b) ""
                           (map (\(d1, d2) -> show d1 ++ " >-> " ++ show d2) inter_ns)

-- Stage 2: Pack the intermediate representation into System.

build :: EDSLIntermediate s a d -> SB.System s a d
build (ss_ns, ds_ns, as_ns, dom, trans_ns, obser, inter_ns, init) = SB.System {
    SB.initial = init,
    SB.step    = eDSLstep trans_ns,
    SB.obs     = eDSLobs obser,
    SB.dom     = eDSLdom dom,
    SB.policy  = buildPolicy inter_ns
}

buildExists :: ExistsEDSLIntermediate -> ExistsEDSLSystem
buildExists (ExI intermediate) = ExES (ES (build intermediate) intermediate)

-- A single do-it-all function that takes in EDSL description for policy and system and builds SB.System (which includes SB.Policy)

makeSystem :: State PrePolicy () -> State EDSLData () -> ExistsEDSLSystem
makeSystem policyDesc systemDesc = buildExists $ prepare (parsePolicy policyDesc) (parse systemDesc)

-- System functions that work over EDSL identifiers instead of NatSet n

stepEDSL :: ExistsEDSLSystem -> StateId -> ActionId -> Maybe StateId
stepEDSL (ExES sys) from_id action_id = case to of
    Just to_state -> Just (lookupId to_state ss_ns)
    Nothing       -> Nothing
    where (ss_ns, _, as_ns, _, _, _, _, _) = getIntermediate sys
          from = genThing ss_ns from_id
          action = genThing as_ns action_id
          to = (SB.step (getBase sys)) from action

obsEDSL :: ExistsEDSLSystem -> StateId -> DomainId -> ObservationSymbol
obsEDSL (ExES sys) s_id d_id = (SB.obs (getBase sys)) s d
    where (ss_ns, ds_ns, _, _, _, obser, _, _) = getIntermediate sys
          s = genThing ss_ns s_id
          d = genThing ds_ns d_id

domEDSL :: ExistsEDSLSystem -> ActionId -> DomainId
domEDSL (ExES sys) a_id = lookupId domain ds_ns
    where (_, ds_ns, as_ns, _, _, _, _, _) = getIntermediate sys
          a = genThing as_ns a_id
          domain = (SB.dom (getBase sys)) a

runEDSL :: ExistsEDSLSystem -> StateId -> [ActionId] -> Maybe StateId
runEDSL _ s [] = Just s
runEDSL sys s (a:as) = (stepEDSL sys s a) >>= (\x -> runEDSL sys x as)

doRunEDSL :: ExistsEDSLSystem -> [ActionId] -> Maybe StateId
doRunEDSL exEs@(ExES sys) as = runEDSL exEs init as
    where (ss_ns, _, _, _, _, _, _, _) = getIntermediate sys
          init = lookupId (SB.initial (getBase sys)) ss_ns

