#!/usr/bin/bash
set -o errexit -o xtrace -o nounset
# WARNING: missing permission setup
# assuming db and static/tmp permissions are already configured

DESTINATION=/opt/saunahali
USER=saunahali

# scripts directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}"  )" && pwd  )"

# Build

cd $DIR/../

# prevent memory problems in pesutykki by not compiling devel server at the same time
killall yesod -KILL || true

if [[ $* == "--fast" ]]
then
    cabal build
elif [[ $* == "--slow" ]] # Use less memory and cpu
then
    touch Settings/StaticFiles.hs
    cabal clean
    cabal build -j1
else
    touch Settings/StaticFiles.hs
    cabal clean
    cabal build
fi



### Copy files and restart

SECRETS="$DESTINATION/config/client_session_key.aes $DESTINATION/config/settings.yml"

# Directories
sudo mkdir -p $DESTINATION/config || true
sudo mkdir -p $DESTINATION/static/gallery || true


# Files
cd $DIR/../
sudo cp -r static $DESTINATION
sudo cp -r config/*[^~] $DESTINATION/config/
sudo mv $DESTINATION/config/mysettings.yml $DESTINATION/config/settings.yml

# Permissions
sudo chmod og-rwx $SECRETS
sudo chown $USER $SECRETS $DESTINATION $DESTINATION/static/{tmp,gallery}


# Binary

sudo cp $DESTINATION/loyly $DESTINATION/loyly.old || true # backup the old

sudo systemctl stop loyly || true  # stop if running
sudo cp dist/build/loyly/loyly $DESTINATION
sudo systemctl start loyly


