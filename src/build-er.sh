#!/bin/bash

# Install dependencies and build the CLI executable.
cabal update
cabal install containers aeson random erf kdt MonadRandom normaldistribution parallel generic-deriving
ghc -threaded -O2 -with-rtsopts="-N32" -main-is Model.UI.ExperimentRunner Model/UI/ExperimentRunner.hs -o ../bin/exp-runner
