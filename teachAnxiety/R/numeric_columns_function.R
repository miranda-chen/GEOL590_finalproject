##' Determines if columns contain valid numeric values and where non-numeric values are located
##' 
##' @description Function which determines if columns contain valid numeric values and, if not, where non-numeric values are located in the data frame.
##' @usage numeric_columns(x)
##' @param x A data frame
##' @return A data frame representing the data from .csv or .xlsx file
##' 
##' 
##' @export

numeric_columns <- function(data) {
   col.types <- vapply(data, FUN = is.numeric, FUN.VALUE = TRUE)
  # To identify bad columns
  if(FALSE %in% col.types) {
    bad.cols <- which(col.types == FALSE)
    warning(paste0("There is/are non-numeric values in column(s) ", bad.cols))
  }
   print(col.types) 
}
