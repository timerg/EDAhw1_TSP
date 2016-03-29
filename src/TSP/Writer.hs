module TSP.Writer where

import TSP.Types
import TSP.BB

serializeList :: [Step] -> String
serializeList [] = ""
serializeList (s:ss) = (show s) ++ "\n" ++ (serializeList ss)

serializeTSPM :: ([Step], TSPState) -> String
serializeTSPM (a, ts) = serializeList a ++ "\n" ++  (show ts)



