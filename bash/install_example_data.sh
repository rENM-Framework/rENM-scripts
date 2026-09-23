#!/usr/bin/env bash

# set project directory name (same as in create_project_directory.sh)
PROJECT_DIRECTORY="$HOME/rENMtest"
cd "${PROJECT_DIRECTORY}"

# download and unzip example dataset
DATA_URL="https://zenodo.org/records/20750253/files/rENM-Framework-v0.1.0-example-data.zip"
ZIP_FILE="rENM-Framework-v0.1.0-example-data.zip"
curl -L -o "${ZIP_FILE}" "${DATA_URL}"
unzip -o "${ZIP_FILE}"

# create symbolic link
ln -sfn rENM-Framework-v0.1.0-example-data data

# clean up
rm -rf __MACOSX
rm -f "${ZIP_FILE}"
