# ------------------------------------------------------------
# Walker et al. Replication
# 02_subgroup_analysis.R
#
# Purpose:
# Replicate subgroup event-study results by school rank
# and out-of-state applicant share
# ------------------------------------------------------------

pacman::p_load(
  tidyverse,
  here,
  fixest,
  broom
)

load(
  here::here(
    "data",
    "saved data",
    "maindf.RData"
  )
)

  # ------------------------------------------------------------
  # Top 50 universities
  # ------------------------------------------------------------
  
  top50_df <- maindf |>
    filter(
      rank_2023 <= 50
    )

# ------------------------------------------------------------
# Top 50 event-study model
# ------------------------------------------------------------

model_top50 <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = top50_df,
  cluster = ~ state
)

print(summary(model_top50))

# Joint pre-treatment test for Top 50 model
pretrend_top50 <- fixest::wald(
  model_top50,
  keep = "year::2018|year::2019|year::2020"
)

print(pretrend_top50)

# ------------------------------------------------------------
# Extract Top 50 event-study coefficients
# ------------------------------------------------------------

top50_coefs <- broom::tidy(
  model_top50,
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
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100
  ) |>
  select(
    year,
    estimate_pp,
    conf_low_pp,
    conf_high_pp,
    p.value
  ) |>
  arrange(year)

top50_plot_data <- top50_coefs |>
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

top50_plot <- ggplot(
  top50_plot_data,
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
    title = "Top 50 Universities",
    subtitle = "2021 is the omitted reference year",
    x = "Application Year",
    y = "Estimated effect (percentage points)",
    caption = paste0(
      "95% CI | Joint pre-trends test: p = ",
      round(pretrend_top50$p, 3)
    )
  ) +
  theme_minimal(base_size = 12)

top50_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "top50_event_study.png"
  ),
  plot = top50_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Universities ranked 51-100
# ------------------------------------------------------------

rank51_100_df <- maindf |>
  filter(
    rank_2023 > 50,
    rank_2023 <= 100
  )

# ------------------------------------------------------------
# Rank 51-100 event-study model
# ------------------------------------------------------------

model_rank51_100 <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = rank51_100_df,
  cluster = ~ state
)

print(summary(model_rank51_100))

# Joint pre-treatment test for ranks 51-100
pretrend_rank51_100 <- fixest::wald(
  model_rank51_100,
  keep = "year::2018|year::2019|year::2020"
)

print(pretrend_rank51_100)

# ------------------------------------------------------------
# Extract rank 51-100 event-study coefficients
# ------------------------------------------------------------

rank51_100_coefs <- broom::tidy(
  model_rank51_100,
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
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100
  ) |>
  select(
    year,
    estimate_pp,
    conf_low_pp,
    conf_high_pp,
    p.value
  ) |>
  arrange(year)

rank51_100_plot_data <- rank51_100_coefs |>
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

rank51_100_plot <- ggplot(
  rank51_100_plot_data,
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
    title = "Universities Ranked 51-100",
    subtitle = "2021 is the omitted reference year",
    x = "Application Year",
    y = "Estimated effect (percentage points)",
    caption = paste0(
      "95% CI | Joint pre-trends test: p = ",
      round(pretrend_rank51_100$p, 3)
    )
  ) +
  theme_minimal(base_size = 12)

rank51_100_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "rank51_100_event_study.png"
  ),
  plot = rank51_100_plot,
  width = 8,
  height = 5,
  dpi = 300
)
