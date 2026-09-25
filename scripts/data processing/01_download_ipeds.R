# ------------------------------------------------------------
# Walker et al. Replication
# 01_download_ipeds.R
#
# Purpose:
# Download original IPEDS admissions data for 2018-2022
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
)# Download IPEDS Directory files
hd_zip_files <- purrr::map(
  years,
  ~ download_ipeds("HD", .x)
)
# Download IPEDS Fall Enrollment Residence/Migration files
efc_zip_files <- purrr::map(
  years,
  ~ download_ipeds("EF", .x, "C")
)