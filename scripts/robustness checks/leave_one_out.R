# ------------------------------------------------------------
# Walker et al. Extension
# Leave-one-out exercise
# 
# Purpose:
# Test whether the 2022 event-study estimate is driven
# by any single state.
# ------------------------------------------------------------

pacman::p_load(
  tidyverse,
  here,
  fixest,
  broom
)

# Load balanced analysis dataset
load(
  here::here(
    "data",
    "saved data",
    "extension_analysis_df.RData"
  )
)

loo_sample <- extension_analysis_df |>
  filter(
    rank_2023 <= 100
  )

# ------------------------------------------------------------
# Full-sample covariate model
# ------------------------------------------------------------
model_baseline <- fixest::feols(
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
  data = loo_sample,
  cluster = ~ state
)

baseline_2022 <- broom::tidy(
  model_baseline,
  conf.int = TRUE
) |>
  filter(
    stringr::str_detect(term, "year::2022")
  ) |>
  transmute(
    estimate = estimate,
    conf_low = conf.low,
    conf_high = conf.high
  )

# ------------------------------------------------------------
# Leave-one-state-out exercise
# ------------------------------------------------------------
# List of states in analysis sample
states <- sort(
  unique(
    loo_sample$state
  )
)

# Empty object to store results
loo_results <- tibble()

# Re-estimate model after dropping each state
for (s in states) {
  
  # Remove one state
  df_loo <- loo_sample |>
    filter(
      state != s
    )
  
  # Estimate same baseline model
  model_loo <- fixest::feols(
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
    data = df_loo,
    cluster = ~ state
  )
  
  # Extract 2022 treatment coefficient
  coef_2022 <- broom::tidy(
    model_loo,
    conf.int = TRUE
  ) |>
    filter(
      stringr::str_detect(term, "year::2022")
    ) |>
    transmute(
      state_omitted = s,
      estimate = estimate,
      std_error = std.error,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value
    )
  
  # Add to results
  loo_results <- bind_rows(
    loo_results,
    coef_2022
  )
}

#Convert effects to percentage points 
  baseline_pp <- baseline_2022$estimate * 100
  loo_results <- loo_results |>
  mutate(
    estimate_pp = estimate * 100,
    conf_low_pp = conf_low * 100,
    conf_high_pp = conf_high * 100
  )

#Create figure 
loo_plot <- ggplot(
  loo_results,
  aes(
    x = estimate_pp,
    y = reorder(state_omitted, estimate_pp)
  )
) +
  geom_vline(
    xintercept = baseline_pp,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "solid"
  ) +
  geom_errorbar(
    aes(
      xmin = conf_low_pp,
      xmax = conf_high_pp
    ),
    width = 0.15,
    orientation = "y"
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    title = "Leave-One-State-Out Robustness Check",
    subtitle = "2022 event-study treatment effect",
    x = "Estimated effect (percentage points)",
    y = "State omitted",
    caption = "Dashed line = full-sample estimate; bars = 95% confidence intervals"
  ) +
  theme_minimal(base_size = 12)
loo_plot

#save plot 
ggsave(
  filename = here::here(
    "results",
    "figures",
    "leave_one_out.png"
  ),
  plot = loo_plot,
  width = 8,
  height = 8,
  dpi = 300
)

#Save underlying results 
readr::write_csv(
  loo_results,
  here::here(
    "results",
    "tables",
    "leave_one_out.csv"
  )
)

#Find most impactful coefficient  
loo_summary <- loo_results |>
  summarize(
    baseline_pp = baseline_pp,
    min_estimate_pp = min(estimate_pp),
    max_estimate_pp = max(estimate_pp),
    all_negative = all(estimate_pp < 0),
    significant_5pct = sum(p_value < 0.05),
    total_models = n()
  )

#Print summary of results 
print(loo_summary)

#Identify most impactful state exclusion 
most_influential <- loo_results |>
  mutate(
    difference_from_baseline =
      abs(estimate_pp - baseline_pp)
  ) |>
  arrange(desc(difference_from_baseline)) |>
  slice(1)
print(most_influential)
