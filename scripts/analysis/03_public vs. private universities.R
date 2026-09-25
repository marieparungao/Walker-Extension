# ------------------------------------------------------------
# Additional Question:
# Public vs. Private Universities
#
# Does the effect of abortion restrictions on female
# applicant share differ by institutional control?
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
# Inspect institutional control
# ------------------------------------------------------------

maindf |>
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

public_df <- maindf |>
  filter(control == 1)

private_df <- maindf |>
  filter(control == 2)

# ------------------------------------------------------------
# Public university event study
# ------------------------------------------------------------

model_public <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = public_df,
  cluster = ~ state
)

print(summary(model_public))


# ------------------------------------------------------------
# Private nonprofit university event study
# ------------------------------------------------------------

model_private <- fixest::feols(
  wshare ~ i(
    year,
    repeal,
    ref = 2021
  ) |
    unitid + state + year,
  data = private_df,
  cluster = ~ state
)

print(summary(model_private))

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
# ------------------------------------------------------------

extension_df <- maindf |>
   filter(control %in% c(1, 2)) |>
   mutate(
    private = if_else(control == 2, 1L, 0L),
    repeal_private = repeal * private
  )

model_public_private <- fixest::feols(
  wshare ~
    i(year, repeal, ref = 2021) +
    i(year, private, ref = 2021) +
    i(year, repeal_private, ref = 2021) |
    unitid + state + year,
  data = extension_df,
  cluster = ~ state
)

print(summary(model_public_private))

# ------------------------------------------------------------
# Extract public and private event-study estimates
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
      "-0.54 pp, p = 0.161"
    )
  ) +
  theme_minimal(base_size = 12)

public_private_plot

ggsave(
  filename = here::here(
    "results",
    "figures",
    "public_private_extension.png"
  ),
  plot = public_private_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# Save public/private extension results
# ------------------------------------------------------------

extension_results <- tibble(
  group = c(
    "Public",
    "Private nonprofit",
    "Private - Public difference"
  ),
  estimate_pp = c(
    -0.004268 * 100,
    -0.009640 * 100,
    -0.005373 * 100
  ),
  p_value = c(
    0.25278,
    0.000051439,
    0.161482
  )
)

readr::write_csv(
  extension_results,
  here::here(
    "results",
    "tables",
    "public_private_extension.csv"
  )
)

print(extension_results)
