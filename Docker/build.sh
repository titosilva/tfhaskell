#!/bin/env bash

VERSION_STR=$1

echo "Creating tag:"
echo $VERSION_STR

docker build . -t titosilva22/tfhe-haskell:$VERSION_STR
docker push titosilva22/tfhe-haskell:$VERSION_STR