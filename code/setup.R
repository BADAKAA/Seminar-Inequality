rm(list = ls())
palette("Okabe-Ito")

required_pkgs <- c("tidyverse", "readr", "dplyr", "haven", "fixest", "modelsummary", "tinytable") # nolint
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])] # nolint
if (length(missing_pkgs) > 0) {
  message("Installing missing R packages: ", paste(missing_pkgs, collapse = ", ")) # nolint
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

source("code/utils.R")
for (dir in DIRS) {
  if (dir.exists(dir)) next
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}