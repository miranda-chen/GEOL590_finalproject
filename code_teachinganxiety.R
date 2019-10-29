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

#load data
getwd() 

setwd("/Users/chen/Desktop/Dissertation Analysis - GTA Anxiety/1- Fall 2016")

data <- read.csv("1 Fall 2016 GTA Teaching Anxiety and Coping Survey copy.csv", header=TRUE) #for Macbook Air

str(data) #check types of data
class(data)
summary(data)
head(data)
colnames(data)
#did blanks become zero??

#Anxiety
#reverse scoring
which(colnames(data)=="Q18_10")
cols = data[,c(39,42,44:45,47:48,51:52,55,58,60,62:63,66)] #check before revser scoring
cols = c("Q18_1", "Q18_4", "Q18_6", "Q18_7", "Q18_9", "Q18_10", "Q19_13", "Q19_14", "Q19_17", "Q19_20", "Q20_22", "Q20_24", "Q20_25","Q20_28")
data[ ,cols] = 6 - data[ ,cols]
cols = data[,c(39,42,44:45,47:48,51:52,55,58,60,62:63,66)] #check after scoring

data["AnxietyScore"] <- NA # That creates the new column named "MY_NEW_COLUMN" filled with "NA"
data$AnxietyScore <- rowSums(data[,39:67], na.rm=T)  # As an example, the new column receives the result of C - D
#coping scores
which(colnames(data)=="Q21_9")
which(colnames(data)=="Q22_18")