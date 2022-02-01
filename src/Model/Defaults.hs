
-- | Contains the type definition for the default propagation model and sub-models, allowing external libraries to more 
--   easily invoke them.
module Model.Defaults where

import Model.Definitions
import Model.Mobility.LevyWalk
import Model.Identification.Signal
import Model.Infection.Serial

type DefaultMobilityModel = LevyWalkModel
type DefaultIdModel = SignalIdModel
type DefaultInfectionModel = SerialInfectionModel

type DefaultPropagationModel = PropagationModel DefaultMobilityModel DefaultIdModel DefaultInfectionModel

