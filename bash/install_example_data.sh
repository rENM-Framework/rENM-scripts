#!/usr/bin/env bash

# set project directory name (same as above ...)
PROJECT_DIRECTORY="$HOME/rENMtest"
cd ${PROJECT_DIRECTORY}

# download and unzip example data set
DATA_URL="https://storage.googleapis.com/renm_data/example_data.zip"
ZIP_FILE="example_data.zip"
curl -L -o "${ZIP_FILE}" "${DATA_URL}"
unzip -o "${ZIP_FILE}"

# create symbolic link
ln -sfn example_data data

# clean up the installation
rm -rf __MACOSX
rm -f "$ZIP_FILE"
