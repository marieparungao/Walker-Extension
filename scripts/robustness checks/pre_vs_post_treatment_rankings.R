# ------------------------------------------------------------
# Walker et al. Extension
# Pre- vs. Post-Treatment Rankings Robustness Check
#
# Purpose:
# Test whether the estimated anticipatory effect is sensitive
# to defining the Top-100 university sample using the
# pre-treatment 2021 ranking instead of the original
# post-treatment 2023 ranking.
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
# Create ranking-based analysis samples
# ------------------------------------------------------------

# Original Walker sample using the 2023 ranking
sample_2023 <- extension_analysis_df |>
  filter(
    rank_2023 <= 100
  )

# Robustness sample using the pre-treatment 2021 ranking
sample_2021 <- extension_analysis_df |>
  filter(
    rank_2021 <= 100
  )

# ------------------------------------------------------------
# Full-covariate model: 2023 ranking sample
# ------------------------------------------------------------

# Original Walker sample definition using the post-treatment
# 2023 Top-100 ranking.

model_rank_2023 <- fixest::feols(
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
  data = sample_2023,
  cluster = ~ state
)


# ------------------------------------------------------------
# Full-covariate model: 2021 ranking sample
# ------------------------------------------------------------

# Robustness specification using the pre-treatment
# 2021 Top-100 ranking. All other model components remain
# identical to the 2023-ranking specification.

model_rank_2021 <- fixest::feols(
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
  data = sample_2021,
  cluster = ~ state
)


# ------------------------------------------------------------
# Joint pre-treatment tests
# ------------------------------------------------------------

pretrend_rank_2023 <- fixest::wald(
  model_rank_2023,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
)

pretrend_rank_2021 <- fixest::wald(
  model_rank_2021,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
)

# ------------------------------------------------------------
# Compare 2022 estimates across ranking definitions
# ------------------------------------------------------------

ranking_2022_comparison <- bind_rows(
  broom::tidy(
    model_rank_2023,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      ranking_sample = "2023 Top 100"
    ),
  
  broom::tidy(
    model_rank_2021,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      ranking_sample = "2021 Top 100"
    )
) |>
  transmute(
    ranking_sample,
    estimate_pp = estimate * 100,
    std_error_pp = std.error * 100,
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100,
    p_value = p.value
  ) |>
  mutate(
    pretrend_p = c(
      pretrend_rank_2023$p,
      pretrend_rank_2021$p
    )
  )

print(
  ranking_2022_comparison
)

readr::write_csv(
  ranking_2022_comparison,
  here::here(
    "results",
    "tables",
    "ranking_year_2022_comparison.csv"
  )
)