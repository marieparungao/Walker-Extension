# ------------------------------------------------------------
# Walker et al. Extension
# Institution-Type Placebo / Heterogeneity Check
#
# Purpose:
# Test whether the estimated anticipatory effect differs
# between public and private nonprofit universities.
#
# This helps assess whether the main result may be driven
# disproportionately by one type of institution.
# ------------------------------------------------------------

pacman::p_load(
  tidyverse,
  here,
  fixest,
  broom
)

# Load the final extension dataset containing the
# time-varying covariates used in the robustness analysis.
load(
  here::here(
    "data",
    "saved data",
    "extension_analysis_df.RData"
  )
)

# Recreate the original Walker 2023 Top-100 sample.
# This keeps the placebo check comparable with the main
# covariate specification.

analysis_2023 <- extension_analysis_df |>
  filter(
    rank_2023 <= 100
  )

# ------------------------------------------------------------
# Inspect institutional control
# ------------------------------------------------------------

analysis_2023 |>
  distinct(
    unitid,
    control,
    repeal
  ) |>
  count(
    control,
    repeal,
    name = "schools"
  )

# ------------------------------------------------------------
# Create public and private samples
# ------------------------------------------------------------

public_df <- analysis_2023 |>
  filter(
    control == 1
  )

private_df <- analysis_2023 |>
  filter(
    control == 2
  )

# ------------------------------------------------------------
# Public university event study with time-varying covariates
# ------------------------------------------------------------

model_public <- fixest::feols(
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
  data = public_df,
  cluster = ~ state
)

print(
  summary(
    model_public
  )
)

# ------------------------------------------------------------
# Private nonprofit university event study
# with time-varying covariates
# ------------------------------------------------------------

model_private <- fixest::feols(
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
  data = private_df,
  cluster = ~ state
)

print(
  summary(
    model_private
  )
)

# ------------------------------------------------------------
# Joint pre-treatment tests
# ------------------------------------------------------------

pretrend_public <- fixest::wald(
  model_public,
  keep = "year::2018|year::2019|year::2020"
)

pretrend_private <- fixest::wald(
  model_private,
  keep = "year::2018|year::2019|year::2020"
)

print(pretrend_public)
print(pretrend_private)

# ------------------------------------------------------------
# Formal test of public-private difference
# with time-varying covariates
# ------------------------------------------------------------

public_private_df <- analysis_2023 |>
  filter(
    control %in% c(1, 2)
  ) |>
  mutate(
    private = if_else(
      control == 2,
      1L,
      0L
    ),
    repeal_private = repeal * private
  )

model_public_private <- fixest::feols(
  wshare ~
    i(
      year,
      repeal,
      ref = 2021
    ) +
    i(
      year,
      private,
      ref = 2021
    ) +
    i(
      year,
      repeal_private,
      ref = 2021
    ) +
    unemployment_rate +
    real_min_wage +
    state_gdp +
    population +
    acceptance_rate +
    net_price |
    unitid + state + year,
  data = public_private_df,
  cluster = ~ state
)

print(
  summary(
    model_public_private
  )
)

# ------------------------------------------------------------
# Create public/private 2022 results table
# ------------------------------------------------------------

# Extract the 2022 estimate from the separate public model.
public_2022 <- broom::tidy(
  model_public
) |>
  filter(
    term == "year::2022:repeal"
  )

# Extract the 2022 estimate from the separate private model.
private_2022 <- broom::tidy(
  model_private
) |>
  filter(
    term == "year::2022:repeal"
  )

# Extract the formal private-public difference from
# the pooled interaction model.
private_difference_2022 <- broom::tidy(
  model_public_private
) |>
  filter(
    term == "year::2022:repeal_private"
  )

# Create a compact comparison table.
# Public and private estimates come from the separate subgroup
# models. The private-public difference comes from the pooled
# interaction model, which provides the formal test of whether
# the 2022 effects differ by institutional control.

institution_type_results <- tibble(
  group = c(
    "Public",
    "Private nonprofit",
    "Private - Public difference"
  ),
  estimate_pp = c(
    public_2022$estimate * 100,
    private_2022$estimate * 100,
    private_difference_2022$estimate * 100
  ),
  p_value = c(
    public_2022$p.value,
    private_2022$p.value,
    private_difference_2022$p.value
  )
)

print(
  institution_type_results
)

# ------------------------------------------------------------
# Extract event-study estimates for plotting
# ------------------------------------------------------------

public_coefs <- broom::tidy(
  model_public,
  conf.int = TRUE
) |>
  filter(
    stringr::str_detect(term, "^year::")
  ) |>
  mutate(
    year = as.integer(
      stringr::str_extract(term, "\\d{4}")
    ),
    group = "Public",
    estimate_pp = estimate * 100,
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100
  ) |>
  select(
    year,
    group,
    estimate_pp,
    conf_low_pp,
    conf_high_pp
  )

private_coefs <- broom::tidy(
  model_private,
  conf.int = TRUE
) |>
  filter(
    stringr::str_detect(term, "^year::")
  ) |>
  mutate(
    year = as.integer(
      stringr::str_extract(term, "\\d{4}")
    ),
    group = "Private nonprofit",
    estimate_pp = estimate * 100,
    conf_low_pp = conf.low * 100,
    conf_high_pp = conf.high * 100
  ) |>
  select(
    year,
    group,
    estimate_pp,
    conf_low_pp,
    conf_high_pp
  )

extension_plot_data <- bind_rows(
  public_coefs,
  private_coefs
) |>
  bind_rows(
    tibble(
      year = c(2021, 2021),
      group = c(
        "Public",
        "Private nonprofit"
      ),
      estimate_pp = 0,
      conf_low_pp = 0,
      conf_high_pp = 0
    )
  ) |>
  arrange(
    group,
    year
  )

public_private_plot <- ggplot(
  extension_plot_data,
  aes(
    x = year,
    y = estimate_pp,
    shape = group,
    linetype = group,
    group = group
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
    title = "Public vs. Private Universities",
    subtitle = "2021 is the omitted reference year",
    x = "Application Year",
    y = "Estimated effect (percentage points)",
    shape = "Institution Type",
    linetype = "Institution Type",
    caption = paste0(
      "2022 private-public difference: ",
      round(
        private_difference_2022$estimate * 100,
        2
      ),
      " pp, p = ",
      round(
        private_difference_2022$p.value,
        3
      )
    )
  ) +
  theme_minimal(base_size = 12)

public_private_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "public_private_placebo.png"
  ),
  plot = public_private_plot,
  width = 8,
  height = 5,
  dpi = 300
)

