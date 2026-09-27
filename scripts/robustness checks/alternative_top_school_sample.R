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

# ------------------------------------------------------------
# Joint pre-treatment tests
# ------------------------------------------------------------

pretrend_top100 <- fixest::wald(
  model_top100,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
)

pretrend_top75 <- fixest::wald(
  model_top75,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
)

pretrend_top50 <- fixest::wald(
  model_top50,
  keep = "year::2018|year::2019|year::2020",
  print = FALSE
)

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
  ) |>
  mutate(
    pretrend_p = c(
      pretrend_top100$p,
      pretrend_top75$p,
      pretrend_top50$p
    )
  )

print(
  top_sample_2022_comparison
)

readr::write_csv(
  top_sample_2022_comparison,
  here::here(
    "results",
    "tables",
    "alternative_top_school_2022_comparison.csv"
  )
)
# ------------------------------------------------------------
# Presentation figure: 2022 estimates by Top-school cutoff
# ------------------------------------------------------------

top_sample_plot <- ggplot(
  top_sample_2022_comparison,
  aes(
    x = estimate_pp,
    y = factor(
      sample,
      levels = c(
        "Top 100",
        "Top 75",
        "Top 50"
      )
    )
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(
      xmin = conf_low_pp,
      xmax = conf_high_pp
    ),
    width = 0.12,
    orientation = "y"
  ) +
  geom_point(
    size = 2.8
  ) +
  labs(
    title = "Robustness to Alternative Top-School Cutoffs",
    subtitle = "2022 treatment effect with full time-varying covariates",
    x = "Estimated effect (percentage points)",
    y = "Sample definition",
    caption = "Bars show 95% confidence intervals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.caption = element_text(
      hjust = 0.5
    )
  )

top_sample_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "alternative_top_school_2022.png"
  ),
  plot = top_sample_plot,
  width = 7,
  height = 4.5,
  dpi = 300
)
