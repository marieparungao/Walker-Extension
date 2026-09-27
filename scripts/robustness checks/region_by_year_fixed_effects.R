# ------------------------------------------------------------
# Walker et al. Extension
# Region by Year Fixed Effects
#
# Purpose:
# Test robustness of the covariate model to
# region-by-year fixed effects
# ------------------------------------------------------------

# 1. SETUP ----------------------------------------------------------------

pacman::p_load(tidyverse,here,fixest,broom)

load(
  here::here(
    "data",
    "saved data",
    "extension_analysis_df.RData"
  )
)

# Recreate original 2023 top-100 sample
region_sample <- extension_analysis_df |>
  filter(rank_2023<=100)

# 2. CHECK SAMPLE ----------------------------------------------------------

names(region_sample)

region_sample |>
  count(region,region_name)

region_sample |>
  filter(is.na(region)) |>
  distinct(unitid,state)

n_distinct(region_sample$unitid)
nrow(region_sample)
table(region_sample$repeal,useNA="ifany")

# 3. ALL COVARIATES MODEL --------------------------------------------------------

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
  data = region_sample,
  cluster = ~ state
)

# 4. REGION-BY-YEAR FE MODEL ----------------------------------------------

model_region_year <- fixest::feols(
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
    unitid + region^year,
  data = region_sample,
  cluster = ~ state
)

# 5. EXTRACT RESULTS -------------------------------------------------------

baseline_coefs <- broom::tidy(
  model_baseline,
  conf.int=TRUE
) |>
  filter(str_detect(term,"^year::")) |>
  mutate(
    specification="Baseline",
    year=as.integer(str_extract(term,"\\d{4}")),
    estimate_pp=estimate*100,
    std_error_pp=std.error*100,
    conf_low_pp=conf.low*100,
    conf_high_pp=conf.high*100
  )
region_coefs <- broom::tidy(
  model_region_year,
  conf.int=TRUE
) |>
  filter(str_detect(term,"^year::")) |>
  mutate(
    specification="Region-by-Year FE",
    year=as.integer(str_extract(term,"\\d{4}")),
    estimate_pp=estimate*100,
    std_error_pp=std.error*100,
    conf_low_pp=conf.low*100,
    conf_high_pp=conf.high*100
  )
region_fe_results <- bind_rows(
  baseline_coefs,
  region_coefs
)

# 6. RESULTS TABLE ---------------------------------------------------------

region_fe_table <- region_fe_results |>
  filter(year==2022) |>
  transmute(
    specification,
    estimate_pp=round(estimate_pp,3),
    std_error_pp=round(std_error_pp,3),
    conf_low_pp=round(conf_low_pp,3),
    conf_high_pp=round(conf_high_pp,3),
    p_value=round(p.value,3)
  )

print(region_fe_table)

# 7. PRE-TREND TEST --------------------------------------------------------

pretrend_baseline <- fixest::wald(
  model_baseline,
  keep="year::2018|year::2019|year::2020",
  print = FALSE
)

pretrend_region <- fixest::wald(
  model_region_year,
  keep="year::2018|year::2019|year::2020",
  print = FALSE
)

# 8. FIGURE ---------------------------------------------------------------

region_plot_data <- region_fe_results |>
  select(
    year,
    specification,
    estimate_pp,
    conf_low_pp,
    conf_high_pp
  ) |>
  bind_rows(
    tibble(
      year=c(2021,2021),
      specification=c("All covariates","Region-by-Year FE"),
      estimate_pp=0,
      conf_low_pp=0,
      conf_high_pp=0
    )
  ) |>
  arrange(specification,year)
region_fe_plot <- ggplot(
  region_plot_data,
  aes(
    x=year,
    y=estimate_pp,
    group=specification,
    shape=specification
  )
) +
  geom_hline(
    yintercept=0,
    linetype="dashed"
  ) +
  geom_line(
    position=position_dodge(width=0.1)
  ) +
  geom_point(
    size=2.5,
    position=position_dodge(width=0.1)
  ) +
  geom_errorbar(
    aes(
      ymin=conf_low_pp,
      ymax=conf_high_pp
    ),
    width=0.05,
    position=position_dodge(width=0.1)
  ) +
  scale_x_continuous(
    breaks=2018:2022
  ) +
  labs(
    title="Robustness to Region-by-Year Fixed Effects",
    subtitle="Covariate-adjusted model versus region-by-year specification",
    x="Application Year",
    y="Estimated effect (percentage points)",
    shape="Specification"
  ) +
  theme_minimal(base_size=12)

region_fe_plot
# 9. SAVE OUTPUTS ---------------------------------------------------------

ggsave(
  here::here(
    "results",
    "figures",
    "region_year_fe.png"
  ),
  plot=region_fe_plot,
  width=8,
  height=5,
  dpi=300
)

readr::write_csv(
  region_fe_table,
  here::here(
    "results",
    "tables",
    "region_year_fe.csv"
  )
)

readr::write_csv(
  region_fe_results,
  here::here(
    "results",
    "tables",
    "region_year_fe_full.csv"
  )
)
