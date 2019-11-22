##########
# Package for data processing of Parsons (1973) Teaching Anxiety Survey#
# Oct 29th, 2019#
##########

##Install necessary Packages##
install.packages("doBy")
install.packages("car")
install.packages("vegan")
install.packages("moments")
install.packages("packfor") #still not working for this version of R
install.packages("packfor", repos="http://R-Forge.R-project.org")
install.packages("ade4")
install.packages("vioplot")
install.packages("sm") 
install.packages("sm", repos="https://cloud.r-project.org/src/contrib/sm_2.2-5.6.tar.gz")
install.packages("lsr")
install.packages("plyr")

install.packages("lavaan")
install.packages("psych")
install.packages("nFactors")
install.packages("corrplot")
install.packages("GPArotation")
install.packages("githubinstall")
install.packages("psych")
install.packages("semPlot")

library(doBy) 
library(stats)
library(car)
library(vegan) #need package?
library(MASS)
library(moments)
library(packfor)
library(ade4)
library(vioplot)
library(lsr)
library(plyr)
library(dplyr)

library(lavaan)
library(psych)
library(nFactors)
library(corrplot)
library(GPArotation)
library(githubinstall)
githubinstall("lavaan") 
library(psych)
library(semPlot)
library(tidyverse)

#load data

rm(list = ls(all.names = TRUE)) 

data_practice <- read.csv("test_data.csv", header=TRUE) #for Macbook Air

str(data) #check types of data
class(data)
summary(data)
head(data)
colnames(data)
#did blanks become zero??

# If the read.csv gives and error, let me know. Need to go back into data file to SAVE it as a .csv file first 

## Make sure your save your data as a .csv file 

teach_anxiety <- function(x) {
  # test whether x is logical 
  if(is.data.frame(x) == F) {
    warning("This is not a data frame") #pass a message when it's okay, warning if something is wrong
  }
  
  # if(is.numeric(x) ==  T){
  #    warning("These variables are not numeric") #pass a message when it's okay, warning if something is wrong
  #  }
  
  else {
    
    reversed_x <- x %>%
      mutate(Q1 = 6 - Q1,
             Q4 = 6 - Q4,
             Q6 = 6 - Q6,
             Q7 = 6 - Q7,
             Q9 = 6 - Q9,
             Q10 = 6 - Q10,
             Q13 = 6 - Q13,
             Q14 = 6 - Q14,
             Q17 = 6 - Q17,
             Q20 = 6 - Q20,
             Q22 = 6 - Q22,
             Q24 = 6 - Q24,
             Q25 = 6 - Q25,
             Q28 = 6 - Q28)
    
    
    reversed_x["AnxietyScore"] <- NA # That creates the new column named "MY_NEW_COLUMN" filled with "NA"
    reversed_x$AnxietyScore <- rowSums(reversed_x[ , c("Q1", "Q2", "Q3", "Q4","Q5","Q6","Q7","Q8","Q9",
                                                       "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", 
                                                       "Q17", "Q18", "Q19", "Q20", "Q21", "Q22","Q23",
                                                       "Q24", "Q25", "Q26","Q27","Q28","Q29")], na.rm=T)  # As an example, the new column receives the result of C - D
    
  }
  
  print(reversed_x)
}

teach_anxiety(data_practice)



### Draft functions for anxiety filtering numeric, less than 5 columns blank, survey data###

teach_anxiety <- function(x, index.1, index.2) {
  
  col1 <- x[, index.1]
  col2 <- x[, index.2]
  
  # test whether x is logical 
  if(is.data.frame(x) == F) {
    warning("This is not a data frame.") #pass a message when it's okay, warning if something is wrong
    new_x <- NA
  } 
  
  if(is.numeric(col1) == F & (is.numeric(col2) == F)) {
    warning("Columns are not numeric.") #pass a message when it's okay, warning if something is wrong
    new_x <- NA 
    
  } else {
    
    new_x <- mutate(x, sum.col =  col1 +  col2) #adding two of those indexed columns
  }
  #last statement is what the function returns
  print(new_x)
} 

# need to use 
test <- read.csv("test.xlsx")  # to test if it's a csv file?

if(read.csv("test.xlsx") == F) {
  warning("Columns are not numeric.") #pass a message when it's okay, warning if something is wrong
  new_x <- NA 
} else {
  
} 
?read.csv

is.na() # to test if it's blank?


### test with practice code ###

# Create a vector of Excel files to read
files.to.read = list.files(pattern="xlsx")

# Read each file and write it to csv
lapply(files.to.read, function(f) {
  df = read.xlsx(f, sheet=1)
  write.csv(df, gsub("xlsx", "csv", f), row.names=FALSE)
})

??read.xlsx()

install.packages("xlsx")
library(xlsx)


#If the is.numeric gives me an error, let me know. 
?sapply()
is.numeric(data_practice[, 1:22])
apply(data_practice, 1, mean)
sapply(data_practice, class)
tapply(data_practice$Participant, is.numeric)
col.types <- vapply(data_practice, class, "Q1")
col.types <- is.mu
is.numeric(col.types)
is.character(col.types)
is.data.frame(data_practice[, 1:22])

col.types <- data_practice[, 1:22]

class(data_practice[, 1:22])
is.integer(col.types)
class(cols)


