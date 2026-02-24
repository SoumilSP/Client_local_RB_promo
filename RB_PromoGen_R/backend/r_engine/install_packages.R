# Install required R packages for PromoGen
packages <- c(
  "plumber",
  "jsonlite",
  "data.table",
  "dplyr",
  "tidyr",
  "lubridate",
  "readxl",
  "stringr",
  "reshape2",
  "zoo"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  } else {
    cat("Already installed:", pkg, "\n")
  }
}

cat("\nAll packages installed successfully!\n")
