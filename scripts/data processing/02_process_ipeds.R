# ------------------------------------------------------------
# Walker et al. Replication
# 02_process_ipeds.R
#
# Purpose:
# Import, clean, and combine IPEDS admissions data
# for 2018-2022
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

# ------------------------------------------------------------
# Inspect IPEDS ZIP contents
# ------------------------------------------------------------

ipeds_folder <- here::here(
  "data",
  "original data",
  "IPEDS"
)

# Find all IPEDS ZIP files
zip_files <- list.files(
  ipeds_folder,
  pattern = "\\.zip$",
  full.names = TRUE
)

# List the files contained inside each ZIP
zip_contents <- purrr::map_dfr(
  zip_files,
  function(zip_file) {
    
    contents <- unzip(
      zip_file,
      list = TRUE
    )
    
    contents |>
      mutate(
        zip_file = basename(zip_file)
      )
  }
)

# Show only the ZIP name and files inside it
zip_contents |>
  select(
    zip_file,
    Name
  ) |>
  as_tibble() |>
  print(n = Inf)

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

efc_folder <- here::here(
  "data",
  "original data",
  "IPEDS",
  "EF-C"
)

dir.create(adm_folder, showWarnings = FALSE)
dir.create(hd_folder, showWarnings = FALSE)
dir.create(efc_folder, showWarnings = FALSE)
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
# Extract revised EF-C files
for (year in years) {
  
  zip_path <- file.path(
    ipeds_folder,
    paste0("EF", year, "C.zip")
  )
  
  file_to_extract <- paste0(
    "ef",
    year,
    "c_rv.csv"
  )
  
  unzip(
    zip_path,
    files = file_to_extract,
    exdir = efc_folder
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
# ------------------------------------------------------------
# Import IPEDS Admissions data
# ------------------------------------------------------------

# Import 2018 revised admissions file for inspection
adm2018_raw <- readr::read_csv(
  here::here(
    "data",
    "original data",
    "IPEDS",
    "ADM",
    "adm2018_rv.csv"
  ),
  show_col_types = FALSE
)
# Clean 2018 admissions data
adm2018 <- adm2018_raw |>
  transmute(
    unitid = as.integer(UNITID),
    year = 2018,
    applicants_total = APPLCN,
    applicants_men = APPLCNM,
    applicants_women = APPLCNW,
    wshare = if_else(
      APPLCNM + APPLCNW > 0,
      APPLCNW / (APPLCNM + APPLCNW),
      NA_real_
    )
  )
# Import and combine admissions data for all study years
df.adm <- purrr::map_dfr(
  years,
  read_adm_year
)

# ------------------------------------------------------------
# Import IPEDS Directory data
# ------------------------------------------------------------

# Import 2018 directory file first for inspection
hd2018_raw <- readr::read_csv(
  here::here(
    "data",
    "original data",
    "IPEDS",
    "HD",
    "hd2018.csv"
  ),
  show_col_types = FALSE
)
# Clean 2018 directory data
hd2018 <- hd2018_raw |>
  transmute(
    unitid = as.integer(UNITID),
    year = 2018,
    institution = INSTNM,
    state = STABBR,
    control = as.integer(CONTROL)
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