% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_read_csv_function.R
\name{my_read_csv_function}
\alias{my_read_csv_function}
\title{Reads csv or xlsx files with meaningful error message}
\usage{
my_read_csv_function(file)

my_read_xlsx_function(file)
}
\arguments{
\item{file}{A filename as a character vector of length 1}
}
\value{
A data frame representing the .csv or .xlsx file
}
\description{
Function to read both .csv and .xlsx files into R.
}
