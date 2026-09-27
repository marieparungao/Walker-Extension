# ------------------------------------------------------------
# Walker et al. Extension
# 02_covariate_analysis.R
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

# ------------------------------------------------------------
# Joint pre-treatment test: baseline model
# ------------------------------------------------------------

pretrend_baseline <- fixest::wald(
  model_baseline_extension,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
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

# ------------------------------------------------------------
# Joint pre-treatment test: covariate model
# ------------------------------------------------------------

pretrend_covariates <- fixest::wald(
  model_covariates,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
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
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
)

# Joint pre-treatment test: school covariates only
pretrend_school <- fixest::wald(
  model_school_covariates,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
)

# ------------------------------------------------------------
# Final comparison of 2022 estimates
# ------------------------------------------------------------

model_2022_comparison <- bind_rows(
  
  broom::tidy(
    model_baseline_extension,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      model = "Baseline",
      pretrend_p = pretrend_baseline$p
    ),
  
  broom::tidy(
    model_state_covariates,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      model = "State covariates",
      pretrend_p = pretrend_state$p
    ),
  
  broom::tidy(
    model_school_covariates,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      model = "School covariates",
      pretrend_p = pretrend_school$p
    ),
  
  broom::tidy(
    model_covariates,
    conf.int = TRUE
  ) |>
    filter(
      term == "year::2022:repeal"
    ) |>
    mutate(
      model = "All covariates",
      pretrend_p = pretrend_covariates$p
    )
  
) |>
  transmute(
    model,
    estimate_pp = estimate * 100,
    std_error_pp = std.error * 100,
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100,
    p_value = p.value,
    pretrend_p
  )

print(
  model_2022_comparison
)

readr::write_csv(
  model_2022_comparison,
  here::here(
    "results",
    "tables",
    "covariate_2022_comparison.csv"
  )
)

# ------------------------------------------------------------
# Presentation figure: baseline vs. full-covariate model
# ------------------------------------------------------------

# Combine the event-study estimates from the baseline and
# full-covariate models.
covariate_plot_data <- bind_rows(
  baseline_extension_coefs |>
    mutate(
      model = "Baseline"
    ),
  
  covariate_coefs |>
    mutate(
      model = "All covariates"
    )
) |>
  select(
    year,
    model,
    estimate_pp,
    conf_low_pp,
    conf_high_pp
  ) |>
  
  # Add the omitted 2021 reference year for both models.
  bind_rows(
    tibble(
      year = c(2021, 2021),
      model = c(
        "Baseline",
        "All covariates"
      ),
      estimate_pp = 0,
      conf_low_pp = 0,
      conf_high_pp = 0
    )
  ) |>
  arrange(
    model,
    year
  )

# Plot the two event-study specifications together.
covariate_comparison_plot <- ggplot(
  covariate_plot_data,
  aes(
    x = year,
    y = estimate_pp,
    shape = model,
    linetype = model,
    group = model
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(
      ymin = conf_low_pp,
      ymax = conf_high_pp
    ),
    width = 0.06,
    position = position_dodge(width = 0.18)
  ) +
  geom_line(
    position = position_dodge(width = 0.18)
  ) +
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.18)
  ) +
  scale_x_continuous(
    breaks = 2018:2022
  ) +
  labs(
    title = "Baseline vs. Covariate-Adjusted Event Study",
    subtitle = "2021 is the omitted reference year",
    x = "Application Year",
    y = "Estimated effect (percentage points)",
    shape = "Model",
    linetype = "Model",
    caption = paste0(
      "Joint pre-trend p-values: Baseline = ",
      round(pretrend_baseline$p, 3),
      "; All covariates = ",
      round(pretrend_covariates$p, 3)
    )
  )
  theme_minimal(base_size = 12)

covariate_comparison_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "covariate_model_comparison.png"
  ),
  plot = covariate_comparison_plot,
  width = 8,
  height = 5,
  dpi = 300
)
