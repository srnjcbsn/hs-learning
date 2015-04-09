module Presentation where
import Learning

data ( Knowledge knl i q
     , Experiment exp w i
     ) => SimState knl exp w i q = SimState
    { step       :: Int
    , knowledge  :: knl
    , env        :: w
    , experiment :: exp
    }

data ( Knowledge knl i q
     , Experiment exp w i
     ) => Log knl exp w i q = Log [SimState knl exp w i q]
