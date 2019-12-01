##' Function to calculate Teaching Anxiety Score from Parson's (1973) survey
##' 
##' @description Function does TWO things: (1) reverse scores 14 questions, and (2) sums all columns for each row/participant to calculate an Anxiety Score.   
##' @usage teach_anxiety(x)
##' @param x A dataframe. NAs are allowed. Use contains_na_function to detect where NAs are located.
##' 
##' @return A data frame with the Teaching Anxiety score as the last column.
##' @details Function to calculate Teaching Anxiety score.
##' 
##' 
##' @export

teach_anxiety <- function(data) {
  # test whether data is in the format of a dataframe 
  if(is.data.frame(data) == F) {
    warning("This is not a data frame") #pass a message when it's okay, warning if something is wrong
  } else {
    # reverse scoring data 
    max.value <- 6 
    reversed_data <- data %>%
      dplyr::mutate(Q1 = max.value - Q1,
             Q4 = max.value - Q4,
             Q6 = max.value - Q6,
             Q7 = max.value - Q7,
             Q9 = max.value - Q9,
             Q10 = max.value - Q10,
             Q13 = max.value - Q13,
             Q14 = max.value - Q14,
             Q17 = max.value - Q17,
             Q20 = max.value - Q20,
             Q22 = max.value - Q22,
             Q24 = max.value - Q24,
             Q25 = max.value - Q25,
             Q28 = max.value - Q28)
    
    # calculate the Anxiety Score in a new column
    reversed_data["AnxietyScore"] <- NA # That creates the new column named "MY_NEW_COLUMN" filled with "NA"
    reversed_data$AnxietyScore <- rowSums(reversed_data[ , c("Q1", "Q2", "Q3", "Q4","Q5","Q6","Q7","Q8","Q9",
                                                             "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                                                             "Q17", "Q18", "Q19", "Q20", "Q21", "Q22","Q23",
                                                             "Q24", "Q25", "Q26","Q27","Q28","Q29")], na.rm=T)  
    
  }
  
  print(reversed_data)
}
