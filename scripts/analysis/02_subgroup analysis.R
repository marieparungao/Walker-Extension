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

# ------------------------------------------------------------
# 2021 out-of-state share
# ------------------------------------------------------------

# State abbreviation to FIPS code lookup
state_fips_lookup <- tibble(
  state = c(
    "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
    "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
    "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
    "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
    "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"
  ),
  home_fips = c(
    1,2,4,5,6,8,9,10,11,12,
    13,15,16,17,18,19,20,21,22,23,
    24,25,26,27,28,29,30,31,32,33,
    34,35,36,37,38,39,40,41,42,44,
    45,46,47,48,49,50,51,53,54,55,56
  )
)

# One home-state record per university
school_home_state <- maindf |>
  distinct(
    unitid,
    state
  ) |>
  left_join(
    state_fips_lookup,
    by = "state"
  )

# ------------------------------------------------------------
# Import 2021 IPEDS residence data
# ------------------------------------------------------------

efc2021_raw <- readr::read_csv(
  here::here(
    "data",
    "original data",
    "IPEDS",
    "EF-C",
    "ef2021c_rv.csv"
  ),
  show_col_types = FALSE
)

# Keep only our analysis schools and attach each school's home-state code
efc2021_sample <- efc2021_raw |>
  transmute(
    unitid = as.integer(UNITID),
    residence_code = as.integer(EFCSTATE),
    freshmen = EFRES01
  ) |>
  inner_join(
    school_home_state,
    by = "unitid"
  )

# Calculate out-of-state share using detailed domestic residence rows
oos_2021 <- efc2021_sample |>
  filter(
    !residence_code %in% c(
      57, 58, 89, 90, 98, 99
    )
  ) |>
  group_by(
    unitid,
    state,
    home_fips
  ) |>
  summarise(
    domestic_known = sum(
      freshmen,
      na.rm = TRUE
    ),
    in_state = sum(
      freshmen[
        residence_code == home_fips
      ],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    out_of_state = domestic_known - in_state,
    oos_share = out_of_state / domestic_known
  )

# ------------------------------------------------------------
# Merge 2021 OOS share into analysis panel
# ------------------------------------------------------------

maindf_oos <- maindf |>
  inner_join(
    oos_2021 |>
      select(
        unitid,
        oos_share
      ),
    by = "unitid"
  )

# Split schools using Walker's 50% threshold
high_oos_df <- maindf_oos |>
  filter(oos_share >= 0.50)

low_oos_df <- maindf_oos |>
  filter(oos_share < 0.50)

# ------------------------------------------------------------
# Out-of-state share event-study models
# ------------------------------------------------------------

# 50% or more out-of-state
model_high_oos <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = high_oos_df,
  cluster = ~ state
)

print(summary(model_high_oos))


# Less than 50% out-of-state
model_low_oos <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = low_oos_df,
  cluster = ~ state
)

print(summary(model_low_oos))

# ------------------------------------------------------------
# Joint pre-treatment tests for OOS models
# ------------------------------------------------------------

pretrend_high_oos <- fixest::wald(
  model_high_oos,
  keep = "year::2018|year::2019|year::2020"
)

pretrend_low_oos <- fixest::wald(
  model_low_oos,
  keep = "year::2018|year::2019|year::2020"
)

print(pretrend_high_oos)
print(pretrend_low_oos)

# ------------------------------------------------------------
# Figure A2: 50% or more out-of-state
# ------------------------------------------------------------

high_oos_coefs <- broom::tidy(
  model_high_oos,
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

high_oos_plot_data <- high_oos_coefs |>
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

high_oos_plot <- ggplot(
  high_oos_plot_data,
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
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 2018:2022
  ) +
  labs(
    title = "50% or More Out-of-State",
    subtitle = "2021 is the omitted reference year",
    x = "Application Year",
    y = "Estimated effect (percentage points)",
    caption = paste0(
      "95% CI | Joint pre-trends test: p = ",
      round(pretrend_high_oos$p, 3)
    )
  ) +
  theme_minimal(base_size = 12)

high_oos_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "high_oos_event_study.png"
  ),
  plot = high_oos_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Figure A4: Less than 50% out-of-state
# ------------------------------------------------------------

low_oos_coefs <- broom::tidy(
  model_low_oos,
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

low_oos_plot_data <- low_oos_coefs |>
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

low_oos_plot <- ggplot(
  low_oos_plot_data,
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
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 2018:2022
  ) +
  labs(
    title = "Less Than 50% Out-of-State",
    subtitle = "2021 is the omitted reference year",
    x = "Application Year",
    y = "Estimated effect (percentage points)",
    caption = paste0(
      "95% CI | Joint pre-trends test: p = ",
      round(pretrend_low_oos$p, 3)
    )
  ) +
  theme_minimal(base_size = 12)

low_oos_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "low_oos_event_study.png"
  ),
  plot = low_oos_plot,
  width = 8,
  height = 5,
  dpi = 300
)
