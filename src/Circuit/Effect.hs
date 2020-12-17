module Circuit.Effect where

import Data.Proxy
import Circuit.Types
import Circuit.Class
import Control.Monad.State.Lazy


--appendM :: (LabelledCircuit circ,
--            AllOccurIn k q2, --All the targets are some output of C
--            CompatibleOver q2 k q3, --D has the right type to be plugged into the target outputs of C
--            RemoveLabels q2 k q7, --q7 = q2 - targets
--            MakeFreshLabels q7 q4 k', --outputs are n fresh labels, where n is the length of q4
--            SemanticOf s q4, --s is the semantics of q4
--            FromLabelsAndSemantics k' s q8, --q8 has the labels of outputs and the semantics of q4
--            --LabelsOf q8 outputs, --q8 has the labels of outputs...
--            --SemanticOf out q8, -- ...with the same semantics as q4 (i.e. wire types)
--            Merge q8 q7 q6)
--                => Proxy k -> circ q3 q4 -> State (circ q1 q6) (Proxy k')
--appendM targets d = do
--    c <- get --TODO the problem is that the signature cannot change within the state due to Monad constraints
--    let (c', k') = append c targets d
--    put c'
--    return k'
--