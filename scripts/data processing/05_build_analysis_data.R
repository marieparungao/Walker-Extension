# ------------------------------------------------------------
# Walker et al. Extension
# 05_build_analysis_data.R
#
# Purpose:
# Join treatment status, rankings, region, and all state-
# and school-level time-varying covariates into one final
# extension analysis dataset.
#
# Main output:
#   extension_analysis_df
# ------------------------------------------------------------
pacman::p_load(
  tidyverse,
  readxl,
  here
)

# Years included in the Walker analysis
years <- 2018:2022

# ------------------------------------------------------------
# Abortion-policy treatment information
# ------------------------------------------------------------

# Recreate the same state-level treatment indicator used
# in the original Walker replication.

policy_data <- readxl::read_excel(
  here::here(
    "data",
    "original data",
    "Table 1.xlsx"
  )
) |>
  filter(
    !is.na(state_abbr)
  ) |>
  transmute(
    state = state_abbr,
    policy_status = abortion_legal_status,
    in_sample = in_sample,
    repeal = if_else(
      abortion_legal_status == "Banned",
      1L,
      0L
    )
  )

# ------------------------------------------------------------
# Build final extension analysis dataset
# ------------------------------------------------------------

extension_analysis_df <- extension_base |>
  
  # Add state abortion-policy treatment status.
  left_join(
    policy_data,
    by = "state"
  ) |>
  
  # Add state-year time-varying covariates.
  left_join(
    state_covariates |>
      select(
        state,
        year,
        unemployment_rate,
        population,
        state_gdp,
        real_min_wage
      ),
    by = c(
      "state",
      "year"
    )
  ) |>
  
  # Add school-year acceptance rate.
  left_join(
    acceptance_data |>
      select(
        unitid,
        year,
        acceptance_rate
      ),
    by = c(
      "unitid",
      "year"
    )
  ) |>
  
  # Add school-year institutional net price.
  left_join(
    net_price_data |>
      select(
        unitid,
        year,
        net_price
      ),
    by = c(
      "unitid",
      "year"
    )
  ) |>
  
  # Add IPEDS geographic region.
  left_join(
    region_data,
    by = c(
      "unitid",
      "year"
    )
  ) |>
  
  arrange(
    unitid,
    year
  )

# ------------------------------------------------------------
# Save final extension analysis dataset
# ------------------------------------------------------------

save(
  extension_analysis_df,
  file = here::here(
    "data",
    "saved data",
    "extension_analysis_df.RData"
  )
)
