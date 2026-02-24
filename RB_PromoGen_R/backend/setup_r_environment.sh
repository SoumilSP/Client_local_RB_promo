#!/bin/bash
# R Environment Setup Script
# This script ensures R and required packages are installed and R API is running

LOG_FILE="/var/log/r_setup.log"
R_API_LOG="/var/log/r_api.log"
R_API_PORT=8002

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a $LOG_FILE
}

log "Starting R environment setup..."

# Check if R is installed
if ! command -v Rscript &> /dev/null; then
    log "R not found. Installing R and dependencies..."
    
    # Install R and ALL system dependencies needed for R packages
    apt-get update -qq
    apt-get install -y -qq \
        r-base r-base-dev \
        libsodium-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libicu-dev \
        zlib1g-dev \
        libbz2-dev \
        liblzma-dev \
        libpcre2-dev \
        make \
        g++ \
        > /dev/null 2>&1
    
    if command -v Rscript &> /dev/null; then
        log "R installed successfully: $(R --version | head -1)"
    else
        log "ERROR: R installation failed!"
        exit 1
    fi
else
    log "R is already installed: $(R --version | head -1)"
fi

# Install system dependencies for R packages (in case R was already installed without them)
log "Ensuring system dependencies for R packages..."
apt-get update -qq > /dev/null 2>&1
apt-get install -y -qq libsodium-dev libcurl4-openssl-dev libssl-dev libxml2-dev libicu-dev zlib1g-dev > /dev/null 2>&1

# Check if required R packages are installed
log "Checking R packages..."
PACKAGES_INSTALLED=$(Rscript -e "cat(all(c('plumber', 'jsonlite', 'data.table') %in% installed.packages()[,'Package']))" 2>/dev/null)

if [ "$PACKAGES_INSTALLED" != "TRUE" ]; then
    log "Installing required R packages (this may take a few minutes)..."
    
    # Install packages one by one for better error handling
    Rscript -e "if (!require('jsonlite')) install.packages('jsonlite', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    Rscript -e "if (!require('data.table')) install.packages('data.table', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    Rscript -e "if (!require('plumber')) install.packages('plumber', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    Rscript -e "if (!require('dplyr')) install.packages('dplyr', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    Rscript -e "if (!require('tidyr')) install.packages('tidyr', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    Rscript -e "if (!require('lubridate')) install.packages('lubridate', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    Rscript -e "if (!require('readxl')) install.packages('readxl', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    Rscript -e "if (!require('stringr')) install.packages('stringr', repos='https://cloud.r-project.org/', quiet=TRUE)" 2>&1 | tee -a $LOG_FILE
    
    log "R packages installation completed"
else
    log "All required R packages are already installed"
fi

# Check if R API is running
check_r_api() {
    curl -s "http://localhost:$R_API_PORT/health" > /dev/null 2>&1
    return $?
}

if check_r_api; then
    log "R API is already running on port $R_API_PORT"
else
    log "Starting R API..."
    
    # Kill any existing R processes
    pkill -f "plumber" 2>/dev/null || true
    pkill -f "Rscript.*start_api" 2>/dev/null || true
    sleep 2
    
    # Start R API
    cd /app/backend/r_engine
    nohup Rscript start_api.R > $R_API_LOG 2>&1 &
    R_PID=$!
    
    log "R API started with PID $R_PID, waiting for startup..."
    
    # Wait for R API to be ready (max 30 seconds)
    for i in {1..30}; do
        sleep 1
        if check_r_api; then
            log "R API is ready on port $R_API_PORT"
            break
        fi
        if [ $i -eq 30 ]; then
            log "WARNING: R API may not be ready yet. Check $R_API_LOG for details."
        fi
    done
fi

log "R environment setup completed"
