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
upDate <- select(mlp, -c(1,2,3,14))
