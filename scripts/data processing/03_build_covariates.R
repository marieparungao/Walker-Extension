# ------------------------------------------------------------
# Walker et al. Extension
# 03_build_covariates.R
#
# Purpose:
# Construct state- and school-level time-varying covariates
#
# Main outputs:
#   state_covariates
#   acceptance_data
#   net_price_data
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
    # Use these to construct an effective
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
# looks off later

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
# School-level time-varying covariates
# Net price
# ------------------------------------------------------------

# Create a folder for the extracted IPEDS Student Financial
# Aid (SFA) files used to obtain institutional net price.
sfa_folder <- here::here(
  "data",
  "original data",
  "IPEDS",
  "SFA"
)

dir.create(
  sfa_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# Confirm the SFA ZIP files were downloaded by
# 01_download_ipeds.R before attempting extraction.

required_sfa_zip_files <- sprintf(
  "SFA%02d%02d.zip",
  years %% 100,
  (years + 1) %% 100
)

missing_sfa_zip_files <- required_sfa_zip_files[
  !file.exists(
    here::here(
      "data",
      "original data",
      "IPEDS",
      required_sfa_zip_files
    )
  )
]

if (length(missing_sfa_zip_files) > 0) {
  stop(
    "Required SFA ZIP files are missing. ",
    "Run 01_download_ipeds.R first. Missing: ",
    paste(
      missing_sfa_zip_files,
      collapse = ", "
    )
  )
}

# Construct the SFA ZIP filename corresponding to each
# Walker analysis year and extract its contents.
#
# Example:
#   2018 -> SFA1819.zip
#   2022 -> SFA2223.zip

for (year in years) {
  
  sfa_name <- sprintf(
    "SFA%02d%02d",
    year %% 100,
    (year + 1) %% 100
  )
  
  zip_path <- here::here(
    "data",
    "original data",
    "IPEDS",
    paste0(
      sfa_name,
      ".zip"
    )
  )
  
  unzip(
    zip_path,
    exdir = sfa_folder
  )
}

# ------------------------------------------------------------
# Read net price from extracted SFA files
# ------------------------------------------------------------

# IPEDS reports the comparable net-price measure differently
# for public and private institutions:
#
#   NPIST2 = public institutions
#   NPGRN2 = private institutions
#
# We combine the applicable measure into one variable,
# net_price, for use in the extension.

read_net_price_year <- function(year) {
  
  # Build the academic-year SFA file name.
  # Examples:
  #   2018 -> sfa1819
  #   2020 -> sfa2021
  #   2022 -> sfa2223
  sfa_name <- sprintf(
    "sfa%02d%02d",
    year %% 100,
    (year + 1) %% 100
  )
  
  revised_file <- file.path(
    sfa_folder,
    paste0(
      sfa_name,
      "_rv.csv"
    )
  )
  
  regular_file <- file.path(
    sfa_folder,
    paste0(
      sfa_name,
      ".csv"
    )
  )
  
  # Prefer the revised release when it exists.
  sfa_file <- if (
    file.exists(revised_file)
  ) {
    revised_file
  } else {
    regular_file
  }
  
  readr::read_csv(
    sfa_file,
    show_col_types = FALSE
  ) |>
    transmute(
      unitid = as.integer(UNITID),
      year = year,
      
      # Public institutions report net price for students
      # paying the in-state or in-district tuition rate.
      net_price_public = as.numeric(NPIST2),
      
      # Private institutions use the corresponding
      # grant-recipient net-price measure.
      net_price_private = as.numeric(NPGRN2),
      
      # Only one of these measures applies to a given school.
      # coalesce() takes whichever applicable value is present.
      net_price = coalesce(
        net_price_public,
        net_price_private
      )
    )
}

# Apply the same function to all five analysis years
# and combine them into one school-year dataset.
net_price_data <- purrr::map_dfr(
  years,
  read_net_price_year
)
