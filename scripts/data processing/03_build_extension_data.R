# ------------------------------------------------------------
# Walker et al. Extension
# 03_build_extension_data.R
#
# Purpose:
# Add state- and school-level time-varying covariates
# and variables needed for robustness checks to the
# original Walker replication dataset.
#
# This script builds from maindf rather than recreating
# the original replication sample.
# ------------------------------------------------------------

pacman::p_load(
  tidyverse,
  readxl,
  here,
  janitor
)

# Years included in the Walker analysis
years <- 2018:2022

# ------------------------------------------------------------
# Load original Walker replication dataset
# ------------------------------------------------------------

load(
  here::here(
    "data",
    "saved data",
    "maindf.RData"
  )
)

# Check the original dataset before adding anything
dim(maindf)

maindf |>
  summarise(
    schools = n_distinct(unitid),
    states = n_distinct(state),
    years = n_distinct(year)
  )

# ------------------------------------------------------------
# State-level time-varying covariates
# Source: UKCPR National Welfare Data
# ------------------------------------------------------------

state_covariates <- readxl::read_excel(
  here::here(
    "data",
    "original data",
    "ukcpr_national_welfare_data_1980_2024_jan26update.xlsx"
  ),
  sheet = "Data"
) |>
  janitor::clean_names() |>
  
  # Walker analysis period only
  filter(
    year %in% years
  ) |>
  
  # Keep only variables needed for this extension
  transmute(
    
    # state_name contains the two-letter state abbreviation
    # used by IPEDS (e.g., SC, GA, CA)
    state = state_name,
    
    year = as.integer(year),
    
    # State economic controls
    unemployment_rate = unemployment_rate,
    population = population,
    state_gdp = gross_state_product,
    
    # Keep both minimum-wage measures temporarily.
    # We will use these to construct an effective
    # minimum wage before converting it to real dollars.
    federal_min_wage = federal_minimum_wage,
    state_min_wage = state_minimum_wage
  )
# ------------------------------------------------------------
# Construct effective nominal minimum wage
# ------------------------------------------------------------

state_covariates <- state_covariates |>
  mutate(
    
    # Use the higher of the federal and state minimum wage.
    # This prevents states with a statutory minimum below
    # the federal minimum from being assigned the lower value.
    effective_min_wage = pmax(
      federal_min_wage,
      state_min_wage
    )
  )

# ------------------------------------------------------------
# Inflation adjustment for minimum wage
# ------------------------------------------------------------

# Download monthly CPI-U data from FRED.
# CPI is used to convert the nominal minimum wage
# into constant dollars.
cpi_monthly <- readr::read_csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCNS",
  show_col_types = FALSE
)

# Rename the two columns so the rest of the code does not
# depend on FRED's original column names.
names(cpi_monthly)[1:2] <- c(
  "date",
  "cpi"
)

# Convert monthly CPI observations into annual averages.
cpi_annual <- cpi_monthly |>
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y"))
  ) |>
  filter(
    year %in% years
  ) |>
  group_by(year) |>
  summarise(
    cpi = mean(cpi, na.rm = TRUE),
    .groups = "drop"
  )

# Use the final analysis year as the base year
# for expressing real minimum wages.
base_year <- max(years)

base_cpi <- cpi_annual |>
  filter(
    year == base_year
  ) |>
  pull(cpi)

state_covariates <- state_covariates |>
  
  # CPI varies by year, so join using year.
  left_join(
    cpi_annual,
    by = "year"
  ) |>
  
  mutate(
    
    # Convert nominal effective minimum wage
    # into constant dollars from the final
    # year of the analysis period.
    real_min_wage =
      effective_min_wage *
      (base_cpi / cpi)
  )

# ------------------------------------------------------------
# School-level time-varying covariates
# Acceptance rate
# ------------------------------------------------------------

# This function reads one year of the same IPEDS Admissions
# data already used in the original Walker replication.
#
# We only keep the variables needed to construct acceptance rate:
#   APPLCN = total number of applicants
#   ADMSSN = total number admitted
#
# Acceptance rate = admitted / applicants
# Kept numerator and denominator in case something
# Looks off later
read_acceptance_year <- function(year) {
  
  adm_file <- here::here(
    "data",
    "original data",
    "IPEDS",
    "ADM",
    paste0(
      "adm",
      year,
      "_rv.csv"
    )
  )
  
  readr::read_csv(
    adm_file,
    show_col_types = FALSE
  ) |>
    transmute(
      unitid = as.integer(UNITID),
      year = year,
      applicants = as.numeric(APPLCN),
      admitted = as.numeric(ADMSSN),
      
      # Avoid dividing by zero if a school reports
      # zero applicants in a given year.
      acceptance_rate = if_else(
        applicants > 0,
        admitted / applicants,
        NA_real_
      )
    )
}
# Apply the acceptance-rate function to every
# year in the Walker analysis period.
acceptance_data <- purrr::map_dfr(
  years,
  read_acceptance_year
)

# ------------------------------------------------------------
# Restrict acceptance-rate data to Walker sample
# ------------------------------------------------------------

acceptance_walker <- maindf |>
  
  # Start with the exact school-year observations
  # already contained in the replication dataset.
  select(
    unitid,
    year
  ) |>
  
  # Add acceptance rate using the school and year.
  left_join(
    acceptance_data,
    by = c(
      "unitid",
      "year"
    )
  )

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
# Restrict region information to Walker sample
# ------------------------------------------------------------

region_walker <- maindf |>
  
  # Start with the exact observations from the
  # original replication sample.
  select(
    unitid,
    year
  ) |>
  
  # Add the institution's IPEDS region.
  left_join(
    region_data,
    by = c(
      "unitid",
      "year"
    )
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

rank_compare <- rank_data |>
  mutate(
    
    # Indicates whether the university is part of
    # the pre-treatment 2021 top-100 sample.
    top100_2021 =
      !is.na(rank_2021) &
      rank_2021 <= 100,
    
    # Indicates whether the university is part of
    # the original 2023 top-100 sample.
    top100_2023 =
      !is.na(rank_2023) &
      rank_2023 <= 100
  )
rank_compare |>
  count(
    top100_2021,
    top100_2023
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
# Rebuild IPEDS directory data before ranking restriction
# ------------------------------------------------------------

# Reuse the same directory-cleaning function used in the
# original Walker replication.
source(
  here::here(
    "R",
    "read_hd_year.R"
  )
)

# Combine directory data for all five analysis years.
# This gives us school characteristics such as state,
# institution name, and institutional control before
# applying either the 2021 or 2023 ranking cutoff.
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
