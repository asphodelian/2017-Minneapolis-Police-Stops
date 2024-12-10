#############
# Libraries #
#############

library(caret)
library(caTools)
library(cluster)
library(corrr)
library(datasets)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(fpc)
library(glmnet)
library(ggcorrplot)
library(ggplot2)
library(leaps)
library(lubridate)
library(magrittr)
library(party)
library(psych)
library(readr)
library(readxl)
library(rpart)
library(rpart.plot)
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
policeDate <- transform(police, 
                        day = format(date, "%d"), 
                        month = format(date, "%m"), 
                        year = format(date, "%Y"))

polDate <- subset(policeDate, select = -c(year, day))
# problem into 1,0
policeDate$problem <- ifelse(polDate$problem == "traffic",1,0)


##############
# Downsample #
##############

cite <- polDate
# Converting citationIssued to yes/no
cite$citationIssued <- ifelse(cite$citationIssued == 0, "No", "Yes")
cite$citationIssued <- as.factor(cite$citationIssued)

newDate <- downSample(cite, cite$citationIssued)
summary(newDate)
names(newDate)
dim(newDate)

# column removal
upDate <- select(newDate, -c(1,2,3,14))

##########
# Tables #
##########

problem <- table(upDate$problem)
problem <- as.data.frame(problem)

citation <- table(upDate$citationIssued)
citation <- as.data.frame(citation)

person <- table(upDate$personSearch)
person <- as.data.frame(person)

vehicle <- table(upDate$vehicleSearch)
vehicle <- as.data.frame(vehicle)

preRace <- table(upDate$preRace)
preRace <- as.data.frame(preRace)

race <- table(upDate$race)
race <- as.data.frame(race)

gender <- table(upDate$gender)
gender <- as.data.frame(gender)

precinct <- table(upDate$policePrecinct)
precinct <- as.data.frame(precinct)

neighbor <- table(upDate$neighborhood)
neighbor <- as.data.frame(neighbor)

month <- table(upDate$month)
month <- as.data.frame(month)





