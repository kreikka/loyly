#!/usr/bin/bash
set -o errexit -o xtrace -o nounset

DESTINATION=/opt/saunahali/

# scripts directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}"  )" && pwd  )"


# Build

cd $DIR/../
cabal clean
cabal build


# Copy files and restart

sudo mkdir -p $DESTINATION || true

sudo cp -r $DIR/../{static,config} $DESTINATION

sudo systemctl stop loyly || true  # stop if running
sudo cp $DIR/../dist/build/loyly/loyly $DESTINATION
sudo systemctl start loyly


