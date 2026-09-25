download_ipeds <- function(prefix, year, suffix = "") {
  
  # Name of the IPEDS file, e.g. ADM2018
  file_name <- paste0(prefix, year, suffix)
  
  # Folder where original IPEDS data will be stored
  raw_folder <- here::here(
    "data",
    "original data",
    "IPEDS"
  )
  
  # Create folder if it does not already exist
  dir.create(
    raw_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Location of downloaded ZIP file
  zip_file <- file.path(
    raw_folder,
    paste0(file_name, ".zip")
  )
  
  # NCES download URL
  url <- paste0(
    "https://nces.ed.gov/ipeds/datacenter/data/",
    file_name,
    ".zip"
  )
  
  # Download if the file does not exist
  if (!file.exists(zip_file)) {
    
    message("Downloading ", file_name)
    
    curl::curl_download(
      url = url,
      destfile = zip_file
    )
  }
  
  # Check that the downloaded file is a valid ZIP
  zip_check <- try(
    unzip(zip_file, list = TRUE),
    silent = TRUE
  )
  
  if (inherits(zip_check, "try-error")) {
    stop(
      "The downloaded file for ",
      file_name,
      " is not a valid ZIP file."
    )
  }
  
  return(zip_file)
}