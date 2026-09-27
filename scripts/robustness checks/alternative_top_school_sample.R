# ------------------------------------------------------------
# Walker et al. Extension
# Alternative Top-School Sample Robustness Check
#
# Purpose:
# Test whether the estimated anticipatory effect is sensitive
# to the ranking cutoff used to define a "top" university.
#
# The original Walker sample uses the 2023 Top 100.
# This robustness check keeps the ranking year fixed at 2023
# but tests alternative definitions of "top."
# ------------------------------------------------------------

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
# Create alternative Top-school samples
# ------------------------------------------------------------

# Original Walker-style benchmark
sample_top100 <- extension_analysis_df |>
  filter(
    rank_2023 <= 100
  )

# Primary alternative cutoff
sample_top75 <- extension_analysis_df |>
  filter(
    rank_2023 <= 75
  )

# More restrictive sensitivity check
sample_top50 <- extension_analysis_df |>
  filter(
    rank_2023 <= 50
  )

# ------------------------------------------------------------
# Full-covariate model: Top 100 benchmark
# ------------------------------------------------------------

model_top100 <- fixest::feols(
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
  data = sample_top100,
  cluster = ~ state
)


# ------------------------------------------------------------
# Full-covariate model: Top 75
# ------------------------------------------------------------

model_top75 <- fixest::feols(
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
  data = sample_top75,
  cluster = ~ state
)


# ------------------------------------------------------------
# Full-covariate model: Top 50
# ------------------------------------------------------------

model_top50 <- fixest::feols(
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
  data = sample_top50,
  cluster = ~ state
)

print(summary(model_top100))
print(summary(model_top75))
print(summary(model_top50))

# ------------------------------------------------------------
# Joint pre-treatment tests
# ------------------------------------------------------------

pretrend_top100 <- fixest::wald(
  model_top100,
  keep = "year::2018|year::2019|year::2020"
)

pretrend_top75 <- fixest::wald(
  model_top75,
  keep = "year::2018|year::2019|year::2020"
)

pretrend_top50 <- fixest::wald(
  model_top50,
  keep = "year::2018|year::2019|year::2020"
)

cat("\n--- Top 100: pre-trend test ---\n")
print(pretrend_top100)

cat("\n--- Top 75: pre-trend test ---\n")
print(pretrend_top75)

cat("\n--- Top 50: pre-trend test ---\n")
print(pretrend_top50)

# ------------------------------------------------------------
# Compare 2022 estimates across Top-school cutoffs
# ------------------------------------------------------------

top_sample_2022_comparison <- bind_rows(
  broom::tidy(
    model_top100,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      sample = "Top 100"
    ),
  
  broom::tidy(
    model_top75,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      sample = "Top 75"
    ),
  
  broom::tidy(
    model_top50,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      sample = "Top 50"
    )
) |>
  transmute(
    sample,
    estimate_pp = estimate * 100,
    std_error_pp = std.error * 100,
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100,
    p_value = p.value
  )

print(
  top_sample_2022_comparison
)
