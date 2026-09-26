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
