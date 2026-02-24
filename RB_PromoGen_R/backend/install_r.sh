#!/bin/bash
# Check if R is installed, if not install it
if ! command -v Rscript &> /dev/null; then
    apt-get update && apt-get install -y r-base libsodium-dev libcurl4-openssl-dev libssl-dev libxml2-dev
    Rscript -e "install.packages(c('plumber', 'jsonlite', 'data.table', 'dplyr', 'tidyr', 'lubridate', 'readxl', 'stringr'), repos='https://cloud.r-project.org/', quiet=TRUE)"
fi
