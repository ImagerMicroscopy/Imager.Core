module AggregateMotorizedStage
where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List

import Equipment
import MiscUtils

data AggregateMotorizedStage = AggregateMotorizedStage ![([StageAxis], EquipmentW)]

instance Equipment AggregateMotorizedStage where
    hasMotorizedStage _ = True
    motorizedStageName _ = "AggregateStage"
    supportedStageAxes (AggregateMotorizedStage as) = concat (map fst as)
    getStagePosition (AggregateMotorizedStage as) =
        let (axes, stages) = unzip as
        in  mapM getStagePosition stages >>= \allPositions ->
            foldr f (-1.0, -1.0, -1.0) (zip axes allPositions)
        where
            f (as, (x, y, z)) (accumX, accumY, accumZ) =
                let x' = if (XAxis `elem` as) then x else accumX
                    y' = if (YAxis `elem` as) then y else accumY
                    z' = if (ZAxis `elem` as) then z else accumZ
                in (x', y', z')
    setStagePosition (AggregateMotorizedStage as) pos =
        forConcurrently_ (map snd as) (\eq -> setStagePosition eq pos)

aggregateMotorizedStagesIfNeeded :: [EquipmentW] -> [EquipmentW]
aggregateMotorizedStagesIfNeeded eqs
    | (length (filter hasMotorizedStage eqs) <= 1) = eqs
    | otherwise = let stages = filter (hasMotorizedStage eqs)
                      withoutStages = filter (not . hasMotorizedStage) eqs
                      zipped = map (\s -> (supportedStageAxes s, s)) stages
                      axes = concat (map fst zipped)
                  in  if (nodups axes)
                      then (AggregateMotorizedStage zipped) : withoutStages
                      else throw (userError "ERROR: more than one motorized stage reports that it can control the same axis")
