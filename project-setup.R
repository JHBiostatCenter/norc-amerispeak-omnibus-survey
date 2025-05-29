# Read required packages from YAML
required_packages <- yaml::read_yaml("packages.yml")$packages

# Load packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    stop(sprintf("Required package '%s' is not installed", pkg))
  }
}

# Source all utility files
source("colors.R")
source("theme-jhu.R")
source("utils.R") # Load utils.R first
source("load-data.R") # Then load-data.R which depends on utils.R
source("table-utils.R")

# Function to load analysis data and create survey design
load_analysis_data <- function() {
  # Check if required data files exist
  required_files <- c(
    "data/analysis_data.rds",
    "data/analysis_meta.rds",
    "data/analysis_tabulations.rds",
    "data/analysis_item_tabulations.rds"
  )

  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    stop(
      "Required data files are missing. Please run 1_norc-data-tabulation.qmd first.\n",
      "Missing files:\n",
      paste("-", missing_files, collapse = "\n")
    )
  }

  # Load saved data from step 1
  data <<- readRDS("data/analysis_data.rds")
  meta <<- readRDS("data/analysis_meta.rds")
  tabulations <<- readRDS("data/analysis_tabulations.rds")
  item_tabulations <<- readRDS("data/analysis_item_tabulations.rds")

  # Create survey design object
  svy_design <<- survey::svydesign(
    ids = ~1, # Cluster ID; none
    data = data,
    weights = ~WEIGHT_EN
  )
}
