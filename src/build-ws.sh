#!/bin/bash

# Install dependencies and build the WebSocket server, which can be used with the UI.
cabal update
cabal install containers aeson random erf kdt MonadRandom normaldistribution parallel generic-deriving
ghc -threaded -O2 -with-rtsopts="-N8" -main-is Model.UI.WebSocket Model/UI/WebSocket.hs -o ../bin/ws-server
