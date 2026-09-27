# ------------------------------------------------------------
# Walker et al. Extension
# Run Full Extension Workflow
#
# Purpose:
# Rebuild the replication/extension data, estimate the main
# covariate model, and run the completed robustness checks
# in the correct dependency order.
#
# COME BACK AND FIX THIS AT THE END
# leave-one-out and region-by-year checks will be
# added after reviewing them together
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
# 5. Build final extension analysis dataset
# ------------------------------------------------------------

# Combines treatment status, rankings, region information,
# and all time-varying covariates into extension_analysis_df.
source(
  here::here(
    "scripts",
    "data processing",
    "05_build_analysis_data.R"
  )
)

# ------------------------------------------------------------
# 6. Run original Walker baseline model
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "analysis",
    "01_main analysis.R"
  )
)
# ------------------------------------------------------------
# 7. Run time-varying covariate analysis
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "analysis",
    "02_covariate_analysis.R"
  )
)

# ------------------------------------------------------------
# 8. Pre- vs. post-treatment ranking robustness check
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "robustness checks",
    "pre_vs_post_treatment_rankings.R"
  )
)
# ------------------------------------------------------------
# 9. Alternative Top-school sample robustness check
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "robustness checks",
    "alternative_top_school_sample.R"
  )
)

# ------------------------------------------------------------
# 10. Institution-type placebo / heterogeneity check
# ------------------------------------------------------------

source(
  here::here(
    "scripts",
    "robustness checks",
    "placebo_institution_type.R"
  )
)
