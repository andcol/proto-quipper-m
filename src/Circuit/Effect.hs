module Circuit.Effect where

import Data.Proxy
import Circuit.LabelContext
import Circuit.Class
import Control.Monad.State.Lazy


--appendM :: Proxy k -> circ q3 q4 -> State (circ q1 q6) (Proxy k')
--appendM targets d = do
--    c <- get --TODO the problem is that the signature cannot change within the state due to Monad constraints
--    let (c', k') = append c targets d
--    put c'
--    return k'
