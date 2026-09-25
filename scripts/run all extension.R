# ------------------------------------------------------------
# Walker Extension
# run_all_extension.R
#
# Purpose:
# Build and run the Walker extension analysis
# ------------------------------------------------------------

pacman::p_load(
  tidyverse,
  readxl,
  janitor,
  here,
  fixest,
  broom
)

# ------------------------------------------------------------
# 1. Build original Walker replication dataset
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "data processing",
    "02_process_ipeds.R"
  )
)

# ------------------------------------------------------------
# 2. Run baseline model
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "analysis",
    "01_main analysis.R"
  )
)
