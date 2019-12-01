##' Locates NAs within the dataset 
##' 
##' @description Function determines if and where NAs are in the dataset.
##' @usage contains.na(x)
##' @param fn A filename as a character vector of length 1
##' @param x A dataframe 
##' 
##' @return Either TRUE or FALSE if there is/are NAs in the dataset, and where ther are
##' 
##' 
##' @export

contains.na <- function(data) {
  if(sum(is.na(data)) > 0 ) {
    # To identify NA columns
    find.na <- is.na(data)
    matrix.na <- which(find.na == TRUE, arr.ind = TRUE)
    warning(paste0(" There is/are NA values in column(s) ", matrix.na[ , "col"], ". "))
    results <- TRUE
  } else {
    results <- FALSE
  }
  print(results)  
}

