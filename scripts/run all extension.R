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
# 1. Download raw IPEDS files
# ------------------------------------------------------------

# Downloads the ADM, HD, and SFA ZIP files needed for the
# replication and extension.
source(
  here::here(
    "scripts",
    "data processing",
    "01_download_ipeds.R"
  )
)

# ------------------------------------------------------------
# 2. Build original Walker replication dataset
# ------------------------------------------------------------

# Extracts ADM and HD files and reconstructs the original
# balanced Walker analysis dataset (maindf).
source(
  here::here(
    "scripts",
    "data processing",
    "02_process_ipeds.R"
  )
)

# ------------------------------------------------------------
# 3. Build time-varying covariates
# ------------------------------------------------------------

# Constructs state-level controls, acceptance rates,
# and institutional net price.
source(
  here::here(
    "scripts",
    "data processing",
    "03_build_covariates.R"
  )
)

# ------------------------------------------------------------
# 4. Build extension and robustness samples
# ------------------------------------------------------------

# Constructs region information, the broader balanced
# extension sample, and alternative ranking samples.
source(
  here::here(
    "scripts",
    "data processing",
    "04_build_extension_sample.R"
  )
)

# ------------------------------------------------------------
# 5. Run original Walker baseline model
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "analysis",
    "01_main analysis.R"
  )
)