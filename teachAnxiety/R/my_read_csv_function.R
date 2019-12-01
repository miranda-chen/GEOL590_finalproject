##' Reads csv or xlsx files with meaningful error message
##' 
##' @description Function to read both .csv and .xlsx files into R.
##' @usage my_read_csv_function(file)
##' @usage my_read_xlsx_function(file)
##' @param file A filename as a character vector of length 1
##' @return A data frame representing the .csv or .xlsx file
##' 
##' 
##' @export

my_read_csv_function <- function(fn, ...) {
  
  d <- tryCatch(
    d <- readr::read_csv(fn, ...),
    warning = function(w) w,
    error = function(e) {
      error.message <- paste0("Either ", fn, " does not exist, or it is not a readable .csv file.")
      stop(error.message)
    }
  )
  d
}


my_read_xlsx_function <- function(fn, ...) {
  
  d <- tryCatch(
    d <- readxl::read_excel(fn, ...),
    warning = function(w) w,
    error = function(e) {
      error.message <- paste0("Either ", fn, " does not exist, or it is not a readable .xlsx file.")
      stop(error.message)
    }
  )
  d
}
