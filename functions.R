# R/functions.R
get_raw_data <- function(file) {
  if(file_ext(file) == "csv") {
    data <- fread(here("Raw data", file))
  } else {
    data <- read_excel(here("Raw data", file),
                  skip = 1, col_types  = c("numeric", "date", "numeric", 
                                           "numeric", "text", "text", "text"))
                  
  }
  return(data)
}