# ------------------------------------------------------------
# Walker et al. Extension
# 04_build_extension_sample.R
#
# Purpose:
# Construct variables and alternative samples needed for
# the extension robustness checks.
#
# Main outputs:
#   region_data
#   extension_base
#   sample_2023
#   sample_2021
# ------------------------------------------------------------

pacman::p_load(
  tidyverse,
  readxl,
  here
)

# Years included in the Walker analysis
years <- 2018:2022

# ------------------------------------------------------------
# Region variable for region-by-year fixed effects
# ------------------------------------------------------------

# IPEDS Directory (HD) files contain OBEREG, a geographic
# region identifier for each institution.
#
# We read this directly from the same HD files already used
# in the original Walker replication instead of manually
# assigning states to regions.

read_region_year <- function(year) {
  
  hd_file <- here::here(
    "data",
    "original data",
    "IPEDS",
    "HD",
    paste0(
      "hd",
      year,
      ".csv"
    )
  )
  
  readr::read_csv(
    hd_file,
    show_col_types = FALSE
  ) |>
    transmute(
      unitid = as.integer(UNITID),
      year = year,
      region = as.integer(OBEREG)
    )
}

# Apply the region-import function to every study year.
region_data <- purrr::map_dfr(
  years,
  read_region_year
)

# ------------------------------------------------------------
# U.S. News rankings for ranking-based robustness checks
# ------------------------------------------------------------

# The original Walker replication selects schools using
# the 2023 U.S. News ranking.
#
# For the extension, retain both the pre-treatment 2021
# ranking and the original 2023 ranking before applying
# any ranking cutoff.

rank_data <- readxl::read_excel(
  here::here(
    "data",
    "original data",
    "USNWR.xlsx"
  )
) |>
  transmute(
    unitid = as.integer(IPEDS),
    university = `University Name`,
    rank_2021 = `2021`,
    rank_2023 = `2023`
  )

# ------------------------------------------------------------
# Define broader ranking universe for extension
# ------------------------------------------------------------

# Keep any university that is top 100 under either the
# 2021 pre-treatment ranking or the original 2023 ranking.
#
# We do this BEFORE constructing ranking-specific samples
# so that schools are not excluded simply because their
# rank changed between 2021 and 2023.

extension_rank_universe <- rank_data |>
  filter(
    rank_2021 <= 100 |
      rank_2023 <= 100
  )

# ------------------------------------------------------------
# Rebuild full IPEDS data before ranking restriction
# ------------------------------------------------------------

# Reuse the same admissions and directory cleaning functions
# used in the original Walker replication.
source(
  here::here(
    "R",
    "read_adm_year.R"
  )
)

source(
  here::here(
    "R",
    "read_hd_year.R"
  )
)

# Combine admissions data for all five analysis years.
adm_all <- purrr::map_dfr(
  years,
  read_adm_year
)

# Combine directory data for all five analysis years.
hd_all <- purrr::map_dfr(
  years,
  read_hd_year
)

# ------------------------------------------------------------
# Combine admissions and directory data
# ------------------------------------------------------------

# Start with the admissions panel and attach school
# characteristics from the IPEDS Directory files.
#
# unitid identifies the university and year identifies
# the observation year, so together they provide the
# correct school-year match.

extension_ipeds <- adm_all |>
  left_join(
    hd_all,
    by = c(
      "unitid",
      "year"
    )
  )
# ------------------------------------------------------------
# Restrict to universities relevant for ranking checks
# ------------------------------------------------------------

# Keep universities that are in the top 100 under either
# the 2021 ranking or the original 2023 ranking.
#
# We attach both rankings now but do NOT yet choose
# between the 2021 and 2023 samples.

extension_prebalance <- extension_ipeds |>
  inner_join(
    extension_rank_universe |>
      select(
        unitid,
        rank_2021,
        rank_2023
      ),
    by = "unitid"
  )

# ------------------------------------------------------------
# Apply original Walker balanced-panel requirement
# ------------------------------------------------------------

# Keep only schools observed in all five analysis years
# with a nonmissing female applicant share in every year.
#
# This matches the balanced-panel rule used in the
# original Walker replication.

extension_balanced_ids <- extension_prebalance |>
  group_by(unitid) |>
  summarise(
    n_years = n_distinct(year),
    complete_wshare = all(!is.na(wshare)),
    .groups = "drop"
  ) |>
  filter(
    n_years == length(years),
    complete_wshare
  ) |>
  pull(unitid)

# ------------------------------------------------------------
# Create balanced extension base
# ------------------------------------------------------------

# Keep only universities that satisfy the same
# balanced-panel requirement as the original replication.
#
# This dataset still contains schools relevant under either
# the 2021 or 2023 top-100 ranking definition. We have not
# yet chosen a ranking year.

extension_base <- extension_prebalance |>
  filter(
    unitid %in% extension_balanced_ids
  ) |>
  arrange(
    unitid,
    year
  )
# ------------------------------------------------------------
# Recreate original 2023 top-100 sample
# ------------------------------------------------------------

# Apply the original Walker ranking definition to the
# broader balanced extension base.
#
# If the extension data were constructed correctly,
# this sample should match the original maindf.

sample_2023 <- extension_base |>
  filter(
    rank_2023 <= 100
  )
# ------------------------------------------------------------
# Create pre-treatment 2021 top-100 sample
# ------------------------------------------------------------

# Use the pre-treatment 2021 ranking instead of the
# original 2023 ranking to define the university sample.

sample_2021 <- extension_base |>
  filter(
    rank_2021 <= 100
  )
