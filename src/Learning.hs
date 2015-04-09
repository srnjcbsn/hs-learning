module Learning where

import Planning.Viewing
import           Control.Monad
import           Data.Maybe
import Text.Show.Pretty

import Debug.Trace

class Knowledge knl info question | knl -> info question where
    analyze :: knl -> info -> knl
    canAnswer :: knl -> question -> Bool

class Show exp => Experiment exp world info | exp -> world info  where
    conduct :: exp -> world -> IO (world, info)

class Experiment exp world info => Strategy strat world question knl exp info | strat -> exp knl question where
    design :: strat -> question -> knl -> IO( Maybe (exp, strat) )
    update :: strat -> exp -> info -> strat

class Inquirable uni question info | question -> info where
    inquire :: uni -> question -> IO (Maybe info)

class Representable r where
    textual :: r -> String

data (Knowledge k i q, Experiment e w i, Representable r) =>
    Repr k e w i q r = Repr (k -> e -> w -> i -> r)

scientificMethod :: ( Show info
                    , Strategy strat world question knl exp info
                    , Experiment exp world info
                    , Inquirable world question info
                    , Knowledge knl info question
                    )
                 => View world
                 -> strat
                 -> knl
                 -> world
                 -> question
                --  -> Repr knl exp world info question r
                 -> IO (knl, world)
scientificMethod view strat knowledge world question =
  do information <- inquire world question
     let knowledge' = fromMaybe knowledge (liftM (knowledge `analyze`)  information )  -- undefined --liftM (analyze knowledge) information
     dres <- design strat question knowledge'
     case dres of
      Just (experiment, strat') -> do
       (world', testdata) <- conduct experiment world

       _ <- trace (ppShow testdata) $ (envChanged view) world'
       let strat'' = update strat' experiment testdata
       let knowledge'' = analyze knowledge' testdata

       if canAnswer knowledge'' question
       then return (knowledge'', world')
       else scientificMethod view strat'' knowledge'' world' question
      Nothing -> return (knowledge', world)
