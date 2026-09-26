# ------------------------------------------------------------
# Walker et al. Extension
# 04_covariate_analysis.R
#
# Purpose:
# Compare the original Walker event-study specification with
# a specification that adds state- and school-level
# time-varying covariates.
#
# The script also includes diagnostic specifications that add
# the state and school covariates separately to identify which
# group is driving changes in the estimated 2022 effect.

pacman::p_load(
  tidyverse,
  here,
  fixest,
  broom
)

# Load final extension analysis dataset
load(
  here::here(
    "data",
    "saved data",
    "extension_analysis_df.RData"
  )
)

# ------------------------------------------------------------
# Recreate original 2023 Top-100 analysis sample
# ------------------------------------------------------------

analysis_2023 <- extension_analysis_df |>
  filter(
    rank_2023 <= 100
  )

# ------------------------------------------------------------
# Reproduce original Walker baseline model
# ------------------------------------------------------------

# Use the same event-study specification as the original
# replication so we can verify that the new extension dataset
# reproduces the baseline result before adding covariates.

model_baseline_extension <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = analysis_2023,
  cluster = ~ state
)

print(
  summary(
    model_baseline_extension
  )
)
# ------------------------------------------------------------
# Extract baseline event-study coefficients
# ------------------------------------------------------------

baseline_extension_coefs <- broom::tidy(
  model_baseline_extension,
  conf.int = TRUE
) |>
  filter(
    stringr::str_detect(
      term,
      "^year::"
    )
  ) |>
  mutate(
    year = as.integer(
      stringr::str_extract(
        term,
        "\\d{4}"
      )
    ),
    estimate_pp = estimate * 100,
    std_error_pp = std.error * 100,
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100
  ) |>
  select(
    year,
    estimate_pp,
    std_error_pp,
    conf_low_pp,
    conf_high_pp,
    p.value
  ) |>
  arrange(year)

print(
  baseline_extension_coefs
)

# ------------------------------------------------------------
# Joint pre-treatment test: baseline model
# ------------------------------------------------------------

pretrend_baseline <- fixest::wald(
  model_baseline_extension,
  keep = "year::2018|year::2019|year::2020"
)

print(
  pretrend_baseline
)

# ------------------------------------------------------------
# Event-study model with time-varying covariates
# ------------------------------------------------------------

# Add the six state- and school-level controls while keeping
# the original Walker sample, treatment definition, fixed
# effects, and state-clustered standard errors unchanged.

model_covariates <- fixest::feols(
  wshare ~
    i(
      year,
      repeal,
      ref = 2021
    ) +
    unemployment_rate +
    real_min_wage +
    state_gdp +
    population +
    acceptance_rate +
    net_price |
    unitid + state + year,
  data = analysis_2023,
  cluster = ~ state
)

print(
  summary(
    model_covariates
  )
)

# ------------------------------------------------------------
# Extract covariate-model event-study coefficients
# ------------------------------------------------------------

covariate_coefs <- broom::tidy(
  model_covariates,
  conf.int = TRUE
) |>
  filter(
    stringr::str_detect(
      term,
      "^year::"
    )
  ) |>
  mutate(
    year = as.integer(
      stringr::str_extract(
        term,
        "\\d{4}"
      )
    ),
    estimate_pp = estimate * 100,
    std_error_pp = std.error * 100,
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100
  ) |>
  select(
    year,
    estimate_pp,
    std_error_pp,
    conf_low_pp,
    conf_high_pp,
    p.value
  ) |>
  arrange(year)

print(
  covariate_coefs
)

# ------------------------------------------------------------
# Joint pre-treatment test: covariate model
# ------------------------------------------------------------

pretrend_covariates <- fixest::wald(
  model_covariates,
  keep = "year::2018|year::2019|year::2020"
)

print(
  pretrend_covariates
)

# ------------------------------------------------------------
# Compare baseline and covariate specifications
# ------------------------------------------------------------

covariate_comparison <- baseline_extension_coefs |>
  select(
    year,
    baseline_estimate_pp = estimate_pp,
    baseline_se_pp = std_error_pp,
    baseline_p = p.value
  ) |>
  left_join(
    covariate_coefs |>
      select(
        year,
        covariate_estimate_pp = estimate_pp,
        covariate_se_pp = std_error_pp,
        covariate_p = p.value
      ),
    by = "year"
  )

print(
  covariate_comparison
)

# ============================================================
# DIAGNOSTIC ANALYSIS
#
# Purpose:
# Determine whether changes in the main estimate and
# pre-treatment pattern are associated primarily with the
# state-level or school-level covariates.
# ============================================================
# ------------------------------------------------------------
# Diagnostic: state-level covariates only
# ------------------------------------------------------------

model_state_covariates <- fixest::feols(
  wshare ~
    i(
      year,
      repeal,
      ref = 2021
    ) +
    unemployment_rate +
    real_min_wage +
    state_gdp +
    population |
    unitid + state + year,
  data = analysis_2023,
  cluster = ~ state
)

# ------------------------------------------------------------
# Diagnostic: school-level covariates only
# ------------------------------------------------------------

model_school_covariates <- fixest::feols(
  wshare ~
    i(
      year,
      repeal,
      ref = 2021
    ) +
    acceptance_rate +
    net_price |
    unitid + state + year,
  data = analysis_2023,
  cluster = ~ state
)

# Joint pre-treatment test: state covariates only
pretrend_state <- fixest::wald(
  model_state_covariates,
  keep = "year::2018|year::2019|year::2020"
)

# Joint pre-treatment test: school covariates only
pretrend_school <- fixest::wald(
  model_school_covariates,
  keep = "year::2018|year::2019|year::2020"
)

print(pretrend_state)
print(pretrend_school)

# ------------------------------------------------------------
# Diagnostic comparison of 2022 estimates
# ------------------------------------------------------------

model_2022_comparison <- bind_rows(
  broom::tidy(model_baseline_extension) |>
    filter(term == "year::2022:repeal") |>
    mutate(model = "Baseline"),
  
  broom::tidy(model_state_covariates) |>
    filter(term == "year::2022:repeal") |>
    mutate(model = "State covariates"),
  
  broom::tidy(model_school_covariates) |>
    filter(term == "year::2022:repeal") |>
    mutate(model = "School covariates"),
  
  broom::tidy(model_covariates) |>
    filter(term == "year::2022:repeal") |>
    mutate(model = "All covariates")
) |>
  transmute(
    model,
    estimate_pp = estimate * 100,
    std_error_pp = std.error * 100,
    p_value = p.value
  )

print(model_2022_comparison)

