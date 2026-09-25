# ------------------------------------------------------------
# Walker et al. Replication
# Run All
#
# Runs the project from data processing through analysis.
# ------------------------------------------------------------

# Load packages used throughout the project
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  readxl,
  janitor,
  here,
  fs,
  curl,
  fixest,
  broom
)

# ------------------------------------------------------------
# 1. Process raw data and build analysis dataset
# ------------------------------------------------------------
# Download raw IPEDS files
source(
  here::here(
    "scripts",
    "data processing",
    "01_download_ipeds.R"
  )
)

# Process raw data
source(
  here::here(
    "scripts",
    "data processing",
    "02_process_ipeds.R"
  )
)

# ------------------------------------------------------------
# 2. Descriptive analysis
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "descriptives",
    "01_descriptives.R"
  )
)

# ------------------------------------------------------------
# 3. Main Walker replication
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "analysis",
    "01_main analysis.R"
  )
)

# ------------------------------------------------------------
# 4. Walker subgroup replications
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "analysis",
    "02_subgroup analysis.R"
  )
)

# ------------------------------------------------------------
# 5. Additional question: public vs. private
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "analysis",
    "03_public vs. private universities.R"
  )
)