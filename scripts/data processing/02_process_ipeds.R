# ------------------------------------------------------------
# Walker et al. Replication
# 02_process_ipeds.R
#
# Purpose:
# Reproduce the original Walker analysis dataset by combining
# IPEDS data, U.S. News rankings, and abortion-policy data.
# ------------------------------------------------------------

# Load packages
pacman::p_load(
  tidyverse,
  janitor,
  here,
  readxl
)

# Load reusable admissions import function
source(
  here::here(
    "R",
    "read_adm_year.R"
  )
)
# Load reusable directory import function
source(
  here::here(
    "R",
    "read_hd_year.R"
  )
)

# Study years
years <- 2018:2022

# Main folder containing downloaded IPEDS ZIP files
ipeds_folder <- here::here(
  "data",
  "original data",
  "IPEDS"
)

# ------------------------------------------------------------
# Extract selected IPEDS files
# ------------------------------------------------------------

# Create folders for each IPEDS component
adm_folder <- here::here(
  "data",
  "original data",
  "IPEDS",
  "ADM"
)

hd_folder <- here::here(
  "data",
  "original data",
  "IPEDS",
  "HD"
)


dir.create(adm_folder, showWarnings = FALSE)
dir.create(hd_folder, showWarnings = FALSE)

# Extract revised ADM files
for (year in years) {
  
  zip_path <- file.path(
    ipeds_folder,
    paste0("ADM", year, ".zip")
  )
  
  file_to_extract <- paste0(
    "adm",
    year,
    "_rv.csv"
  )
  
  unzip(
    zip_path,
    files = file_to_extract,
    exdir = adm_folder
  )
}
# Extract HD files
for (year in years) {
  
  zip_path <- file.path(
    ipeds_folder,
    paste0("HD", year, ".zip")
  )
  
  file_to_extract <- paste0(
    "hd",
    year,
    ".csv"
  )
  
  unzip(
    zip_path,
    files = file_to_extract,
    exdir = hd_folder
  )
}

# ------------------------------------------------------------
# Import U.S. News rankings data
# ------------------------------------------------------------

# Set path to original data folder
data.path <- here::here(
  "data",
  "original data"
)

# Import rankings workbook
df.rank <- readxl::read_excel(
  here::here(
    data.path,
    "USNWR.xlsx"
  )
)
# Keep variables needed for the Walker replication
df.rank <- df.rank |>
  transmute(
    unitid = as.integer(IPEDS),
    university = `University Name`,
    state = State,
    rank_2023 = `2023`
  ) |>
  filter(
    rank_2023 <= 100
  ) |>
  arrange(
    rank_2023,
    university
  )

# Import and combine admissions data for all study years
df.adm <- purrr::map_dfr(
  years,
  read_adm_year
)


# Import and combine directory data for all study years
df.hd <- purrr::map_dfr(
  years,
  read_hd_year
)

# ------------------------------------------------------------
# Import abortion policy data
# ------------------------------------------------------------

df.policy_raw <- readxl::read_excel(
  here::here(
    "data",
    "original data",
    "Table 1.xlsx"
  )
)
# Clean abortion policy data
df.policy <- df.policy_raw |>
  filter(
    !is.na(state_abbr)
  ) |>
  transmute(
    state = state,
    state_abbr = state_abbr,
    policy_status = abortion_legal_status,
    detail = detail,
    in_sample = in_sample,
    repeal = if_else(
      abortion_legal_status == "Banned",
      1L,
      0L
    )
  )
# ------------------------------------------------------------
# Combine IPEDS admissions and directory data
# ------------------------------------------------------------

df.ipeds <- df.adm |>
  left_join(
    df.hd,
    by = c("unitid", "year")
  )

# ------------------------------------------------------------
# Combine IPEDS data with rankings and abortion policy
# ------------------------------------------------------------

df <- df.ipeds |>
  
  # Keep only universities in the 2023 top-100 ranking sample
  inner_join(
    df.rank |>
      select(
        unitid,
        rank_2023
      ),
    by = "unitid"
  ) |>
  
  # Attach state abortion-policy information
  left_join(
    df.policy |>
      select(
        state_abbr,
        policy_status,
        in_sample,
        repeal
      ),
    by = c("state" = "state_abbr")
  )

# ------------------------------------------------------------
# Create balanced analysis panel
# ------------------------------------------------------------

# Identify schools observed in all five study years
balanced_ids <- df |>
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


# Keep only schools with complete 2018-2022 observations
maindf <- df |>
  filter(
    unitid %in% balanced_ids
  ) |>
  arrange(
    unitid,
    year
  )

# ------------------------------------------------------------
# Save final analysis dataset
# ------------------------------------------------------------

save(
  maindf,
  file = here::here(
    "data",
    "saved data",
    "maindf.RData"
  )
)