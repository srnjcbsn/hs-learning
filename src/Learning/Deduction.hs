module Learning.Deduction where

import PDDL.Type
import qualified Data.Set as Set
import              Data.Set (Set)
import qualified Data.List as List
import qualified Data.Map as Map
import              Data.Map (Map)



mapfluents :: (Map Object [Name]) -> [Object] -> Set [Argument]
mapfluents _ [] = Set.fromList []
mapfluents mapper (obj:rest) = Set.fromList []
  where
    restMapped = mapfluents mapper rest
    f o =
      case Map.lookup o mapper of
        Just names -> outp where
          p n = Set.map ((:) $ Ref n) restMapped
          allOfThem = List.map p names
          outp = List.foldr (Set.union) Set.empty allOfThem
        Nothing ->  Set.empty -- [Const o]


unground :: ActionSpec -> [Name] -> Action -> GroundedPredicate -> [FluentPredicate]
unground as consts (_, objs) gp = undefined
  where
    paras = asParas as
    objsMap = Map.fromAscListWith (++) $ List.zip objs paras
