#############
# Libraries #
#############

library(caTools)
library(cluster)
library(corrr)
library(datasets)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(fpc)
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
policeDate <- transform(police, 
                     date = format(date, "%d"), 
                     month = format(date, "%m"), 
                     year = format(date, "%Y"))

polDate <- subset(policeDate, select = -c(date, year))
# problem into 1,0
policeDate$problem <- ifelse(polDate$problem == "traffic",1,0)


# removing columns for PCA
numPol <- subset(policeDate, select = -c(X, idNum, preRace, race, year, date, neighborhood, gender))

