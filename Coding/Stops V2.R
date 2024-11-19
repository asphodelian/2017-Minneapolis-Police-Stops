#############
# Libraries #
#############

library(corrr)
library(caTools)
library(datasets)
library(dplyr)
library(FactoMineR)
library(ggcorrplot)
library(ggplot2)
library(lubridate)
library(magrittr)
library(party)
library(psych)
library(readr)
library(readxl)
library(tidyr)

###########
# Dataset #
###########

police <- read.csv("D:/Coding/R Storage/M748/Project/Data/police.csv", stringsAsFactors = TRUE)
summary(police)
summary(is.na(police))
dim(police)

################
# Data Editing #
################

# Splitting the Date 
police$date <- as.Date(police$date)
polDate <- transform(police, 
                     date = format(date, "%d"), 
                     month = format(date, "%m"), 
                     year = format(date, "%Y"))

# problem into 1,0
polDate$problem <- ifelse(polDate$problem == "traffic",1,0)

# removing columns for PCA
numPol <- subset(polDate, select = -c(X, idNum, preRace, race, year, date, neighborhood, gender))

##############################
# Splitting Data: Train/Test #
##############################

polsample <- sample.split(police, SplitRatio = 0.8)
train <- subset(police, polsample == TRUE)
test <- subset(police, polsample == FALSE)

#################
# Decision Tree #
#################

model1 <- ctree(citationIssued ~ ., train)
plot(model1)

#######
# PCA #
#######
  

