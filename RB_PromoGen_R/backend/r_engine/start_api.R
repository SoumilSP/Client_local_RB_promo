#!/usr/bin/env Rscript
# Start the PromoGen R Plumber API

library(plumber)

# Set port (default 8002 to avoid conflict with FastAPI on 8001)
port <- as.integer(Sys.getenv("R_API_PORT", "8002"))

# Get script directory - use current working directory
# (User should run this from the r_engine folder)
script_dir <- getwd()

# Load and run the API using relative path
api_file <- file.path(script_dir, "plumber_api.R")

# Check if file exists, if not try current directory directly
if (!file.exists(api_file)) {
  api_file <- "plumber_api.R"
}

cat("Loading API from:", api_file, "\n")
pr <- plumber::plumb(api_file)

# Enable CORS
pr$setDocs("swagger")

# Add CORS filter
pr$filter("cors", function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
})

cat("Starting PromoGen R API on port", port, "\n")
pr$run(host = "0.0.0.0", port = port)
