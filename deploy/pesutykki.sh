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

if [[ $1 != "--fast" ]]
    then cabal clean
fi
cabal build



### Copy files and restart

SECRETS="$DESTINATION/config/client_session_key.aes $DESTINATION/config/settings.yml"

# Directories
sudo mkdir -p $DESTINATION/config || true

# Files
cd $DIR/../
sudo cp -r static $DESTINATION
sudo cp -r config/*[^~] $DESTINATION/config/
sudo mv $DESTINATION/config/mysettings.yml $DESTINATION/config/settings.yml

# Permissions
sudo chmod og-rwx $SECRETS
sudo chown $USER $DESTINATION $DESTINATION/static/tmp $SECRETS

# Binary
sudo systemctl stop loyly || true  # stop if running
sudo cp dist/build/loyly/loyly $DESTINATION
sudo systemctl start loyly


