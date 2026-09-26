# ------------------------------------------------------------
# Walker et al. Extension
# 01_download_ipeds.R
#
# Purpose:
# Download IPEDS source files needed for the Walker
# replication and extension (ADM, HD, and SFA) 
# ------------------------------------------------------------


# Load packages
pacman::p_load(
  tidyverse,
  janitor,
  here,
  fs,
  curl
)


# Load reusable functions
source(
  here::here(
    "R",
    "download_ipeds.R"
  )
)


# Years used in the Walker et al. analysis
years <- 2018:2022


# Download IPEDS Admissions files for all study years
adm_zip_files <- purrr::map(
  years,
  ~ download_ipeds("ADM", .x)

)

# Download IPEDS Directory files
hd_zip_files <- purrr::map(
  years,
  ~ download_ipeds("HD", .x)
)

# ------------------------------------------------------------
# Download IPEDS Student Financial Aid files
# ------------------------------------------------------------

# Net price is reported for academic years rather than
# simply by fall year.
#
# Example:
#   Walker year 2018 corresponds to academic year 2018-19,
#   which is stored in IPEDS file SFA1819.
#
# Construct the correct SFA filename automatically so that
# individual years do not need to be hard-coded.

download_sfa <- function(year) {
  
  # Build the IPEDS SFA file name:
  # 2018 -> SFA1819
  # 2019 -> SFA1920
  # 2020 -> SFA2021
  sfa_name <- sprintf(
    "SFA%02d%02d",
    year %% 100,
    (year + 1) %% 100
  )
  
  # IPEDS download URL
  url <- paste0(
    "https://nces.ed.gov/ipeds/datacenter/data/",
    sfa_name,
    ".zip"
  )
  
  # Save ZIP alongside the other original IPEDS files
  destination <- here::here(
    "data",
    "original data",
    "IPEDS",
    paste0(
      sfa_name,
      ".zip"
    )
  )
  
  # Do not download the file again if it already exists.
  if (!file.exists(destination)) {
    curl::curl_download(
      url,
      destination
    )
  }
  
  destination
}

# Download one SFA file for every Walker analysis year.
sfa_zip_files <- purrr::map(
  years,
  download_sfa
)
