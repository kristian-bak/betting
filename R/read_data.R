#' Read data
#' 
read_data <- function() {
  
  url <- "https://emp3ba.am.files.1drv.com/y4mWEAS_Bf8lu49OHzCaXWBY5cZhk21UoGmkSNAdQfYjRkwm2AJh7QR4nivJKaiDn_nTqjbJpalSzOR5VJXhWg6se0BpcPNroyX6gXQyptChgVs3Qz1t6gfhZUR3GpzG2ceuzCFb62o6baGOpvCm-IGdzzCxem8QmNYsYVxy26OeK4"
  file_path <- "inst/data/Betting.xlsx"
  curl::curl_download(url, file_path)
  data <- readxl::read_excel(file_path)
  
  return(data)
  
}

