module QuickSCS.System.GraphVizDrawer where

import QuickSCS.System.Basics
import QuickSCS.System.Arbitrary
import QuickSCS.System.Description

import qualified Data.Map as Map

showPolicyGraphViz :: ExistsEDSLSystem -> String
showPolicyGraphViz (ExES sys) =
                  "digraph edsl_policy {\n" ++
                  "    node [shape = circle];\n" ++
                  show_inter ++ "\n}"
    where (_, ds_ns, _, _, _, _, inter_ns, _) = getEDSLIntermediate sys
          show_inter = foldl (\a b -> a ++ "\n" ++ b) ""
                       (map (\(d1, d2) -> "    " ++ lookupId d1 ds_ns ++ " -> " ++ lookupId d2 ds_ns ++ ";") inter_ns)

showEDSLGraphViz :: ExistsEDSLSystem -> String
showEDSLGraphViz (ExES sys) =
                  "digraph edsl_system {\n" ++
                  "    node [shape = doublecircle]; " ++ lookupId (initial base_sys) ss_ns ++ "\n" ++
                  "    node [shape = circle];\n" ++
                  show_trans ++ "\n}"
    where base_sys = getEDSLBase sys
          (ss_ns, ds_ns, as_ns, dom, trans_ns, obser, inter_ns, _) = getEDSLIntermediate sys
          show_trans = foldl (\a b -> a ++ "\n" ++ b) ""
                       (map (\((from, action), to) -> foldl (\a b -> a ++ "\n" ++ b) "" $ map (\s_ns -> "    " ++
                        lookupId from ss_ns ++ " -> " ++ lookupId s_ns ss_ns ++ " [ label = \"" ++ lookupId action as_ns ++ "\" ];") to) $
                        Map.toList trans_ns)
