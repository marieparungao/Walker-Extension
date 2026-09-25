# ------------------------------------------------------------
# Walker et al. Replication
# 01_main_analysis.R
#
# Purpose:
# Replicate the main Walker et al. event-study model
# ------------------------------------------------------------

# Load packages
pacman::p_load(
  tidyverse,
  here,
  fixest,
  broom
)

# Load final balanced analysis dataset
load(
  here::here(
    "data",
    "saved data",
    "maindf.RData"
  )
)

# ------------------------------------------------------------
# Main TWFE event-study model
# ------------------------------------------------------------

model_main <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = maindf,
  cluster = ~ state
)

# Display regression results
print(summary(model_main))

# ------------------------------------------------------------
# Plot main event-study estimates
# ------------------------------------------------------------

fixest::iplot(
  model_main,
  main = "Female Applicant Share",
  xlab = "Year",
  ylab = "Estimated Treatment Effect"
)
# ------------------------------------------------------------
# Extract event-study coefficients
# ------------------------------------------------------------

event_coefs <- broom::tidy(
  model_main,
  conf.int = TRUE
) |>
  filter(
    stringr::str_detect(term, "^year::")
  ) |>
  mutate(
    year = as.integer(
      stringr::str_extract(term, "\\d{4}")
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

print(event_coefs)

# ------------------------------------------------------------
# Joint test of pre-treatment coefficients
# ------------------------------------------------------------

pretrend_test <- fixest::wald(
  model_main,
  keep = "year::2018|year::2019|year::2020"
)

print(pretrend_test)

# ------------------------------------------------------------
# Create presentation-ready event-study figure
# ------------------------------------------------------------

# Add the omitted 2021 reference year
event_plot_data <- event_coefs |>
  select(
    year,
    estimate_pp,
    conf_low_pp,
    conf_high_pp
  ) |>
  bind_rows(
    tibble(
      year = 2021,
      estimate_pp = 0,
      conf_low_pp = 0,
      conf_high_pp = 0
    )
  ) |>
  arrange(year)
main_event_plot <- ggplot(
  event_plot_data,
  aes(
    x = year,
    y = estimate_pp
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
    width = 0.08
  ) +
  geom_point(
    size = 2.5
  ) +
  scale_x_continuous(
    breaks = 2018:2022
  ) +
  labs(
    title = "Effect of Abortion Restrictions on Female Applicant Share",
    subtitle = "2021 is the omitted reference year",
    x = "Application Year",
    y = "Estimated effect (percentage points)",
    caption = paste0(
      "95% confidence intervals. ",
      "Joint pre-treatment test: p = ",
      round(pretrend_test$p, 3)
    )
  ) +
  theme_minimal(base_size = 12)

main_event_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "main_event_study.png"
  ),
  plot = main_event_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Compare replication estimates with Walker et al. (2023)
# ------------------------------------------------------------

replication_comparison <- tibble(
  year = c(2018, 2019, 2020, 2022),
  
  walker_estimate_pp = c(
    NA,
    0.4,
    0.2,
    -0.9
  ),
  
  walker_se_pp = c(
    0.6,
    0.5,
    0.4,
    0.3
  )
) |>
  left_join(
    event_coefs |>
      select(
        year,
        replication_estimate_pp = estimate_pp,
        replication_se_pp = std_error_pp,
        replication_p = p.value
      ),
    by = "year"
  )
print(replication_comparison)

# ------------------------------------------------------------
# Save main regression results
# ------------------------------------------------------------

readr::write_csv(
  event_coefs,
  here::here(
    "results",
    "tables",
    "main_event_coefficients.csv"
  )
)

readr::write_csv(
  replication_comparison,
  here::here(
    "results",
    "tables",
    "walker_replication_comparison.csv"
  )
)

# ------------------------------------------------------------
# Save comparison tables
# ------------------------------------------------------------

readr::write_csv(
  event_coefs,
  here::here(
    "results",
    "tables",
    "main_event_coefficients.csv"
  )
)

readr::write_csv(
  replication_comparison,
  here::here(
    "results",
    "tables",
    "walker_replication_comparison.csv"
  )
)