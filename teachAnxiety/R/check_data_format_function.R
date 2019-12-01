##' Reads csv with meaningful error message
##' 
##' @description Using the my_read_csv_function and my_read_xlsx_function to check the data format to test if R can open data.
##' @usage check_data_format(file) 
##' @param file A filename as a character vector of length 1
##' @return A data frame containing data from the .csv or .xlsx file
##' 
##' 
##' @export

check_data_format <- function(filename, sheet = "Sheet1", ...) {
  
  # First check whether filename is a character vector
  if(!(is.character(filename))) {
    stop("You didn't pass me a character vector; I can't open this.")
  }
  
  # Check whether hte filename ends in .xlsx
  if(str_detect(filename, "\\.xlsx$")) {
    # In this case, filename ends in .xlsx
    my_obj <- my_read_xlsx_function(filename, ...)
  } else {
    # If filename ends in .csv
    my_obj <- my_read_csv_function(filename, ...)
  }
  #this will display the data to you
  my_obj
}

