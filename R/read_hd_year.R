read_hd_year <- function(year) {
  
  # Build path to IPEDS directory file
  file_path <- here::here(
    "data",
    "original data",
    "IPEDS",
    "HD",
    paste0("hd", year, ".csv")
  )
  
  # Import and clean one year
  readr::read_csv(
    file_path,
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      unitid = as.integer(UNITID),
      year = as.integer(year),
      institution = INSTNM,
      state = STABBR,
      control = as.integer(CONTROL)
    )
}
