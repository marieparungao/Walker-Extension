# ------------------------------------------------------------
# Walker et al. Replication
# 01_descriptives.R
#
# Purpose:
# Generate descriptive statistics for the final analysis sample
# ------------------------------------------------------------

# Load packages
pacman::p_load(
  tidyverse,
  here
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
# Women's applicant share by year and treatment group
# ------------------------------------------------------------

wshare_by_year <- maindf |>
  mutate(
    treatment_group = if_else(
      repeal == 1,
      "Ban state",
      "Control state"
    )
  ) |>
  group_by(
    treatment_group,
    year
  ) |>
  summarise(
    mean_wshare = mean(wshare, na.rm = TRUE),
    schools = n_distinct(unitid),
    .groups = "drop"
  ) |>
  mutate(
    mean_wshare_pct = mean_wshare * 100
  )

print(wshare_by_year)
# ------------------------------------------------------------
# Simple 2021-2022 difference-in-differences
# ------------------------------------------------------------

did_means <- maindf |>
  filter(
    year %in% c(2021, 2022)
  ) |>
  group_by(
    repeal,
    year
  ) |>
  summarise(
    mean_wshare = mean(wshare, na.rm = TRUE),
    .groups = "drop"
  )

print(did_means)
# Calculate change from 2021 to 2022 for each group
did_changes <- did_means |>
  pivot_wider(
    names_from = year,
    values_from = mean_wshare
  ) |>
  mutate(
    change = `2022` - `2021`
  )

print(did_changes)
# Raw difference-in-differences estimate
raw_did <- did_changes |>
  summarise(
    did = change[repeal == 1] -
      change[repeal == 0]
  )

print(raw_did)