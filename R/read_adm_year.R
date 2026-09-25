read_adm_year <- function(year) {
  
  # Build path to revised admissions file
  file_path <- here::here(
    "data",
    "original data",
    "IPEDS",
    "ADM",
    paste0("adm", year, "_rv.csv")
  )
  
  # Import and clean one year
  readr::read_csv(
    file_path,
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      unitid = as.integer(UNITID),
      year = as.integer(year),
      applicants_total = APPLCN,
      applicants_men = APPLCNM,
      applicants_women = APPLCNW,
      wshare = dplyr::if_else(
        APPLCNM + APPLCNW > 0,
        APPLCNW / (APPLCNM + APPLCNW),
        NA_real_
      )
    )
}