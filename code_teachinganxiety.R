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

data <- read.csv("1 Fall 2016 GTA Teaching Anxiety and Coping Survey copy.csv", header=TRUE) #for Macbook Air

str(data) #check types of data
class(data)
summary(data)
head(data)
colnames(data)
#did blanks become zero??

#Anxiety
#reverse scoring
which(colnames(data)=="Q18_10") #checking which column names are linked to which column number
cols = data[,c(39,42,44:45,47:48,51:52,55,58,60,62:63,66)] #choosing the columns which need to be reverse scored
cols = c("Q18_1", "Q18_4", "Q18_6", "Q18_7", "Q18_9", "Q18_10", "Q19_13", "Q19_14", "Q19_17", "Q19_20", "Q20_22", "Q20_24", "Q20_25","Q20_28")
data[ ,cols] = 6 - data[ ,cols] # subract by the max number of likert scales (1-5 in this case) 
cols = data[,c(39,42,44:45,47:48,51:52,55,58,60,62:63,66)] #check after scoring to ensure they are reversed scored

data["AnxietyScore"] <- NA # That creates the new column named "MY_NEW_COLUMN" filled with "NA"
data$AnxietyScore <- rowSums(data[,39:67], na.rm=T)  # As an example, the new column receives the result of C - D

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
  
} )
?read.csv

is.na() # to test if it's blank?





