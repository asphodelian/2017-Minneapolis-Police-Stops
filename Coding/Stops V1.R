#############
# Libraries #
#############

library(dplyr)
library(psych)
library(readr)
library(readxl)
library(tidyr)

###########
# Dataset #
###########

stop <- read.csv("D:/Coding/R Storage/M748/Project/Data/MplsStops.csv")
summary(stop)
dim(stop)

# NA omission
mlp <- stop[!(is.na(stop$citationIssued) | stop$citationIssued == ""), ]
dim(mlp)

# write-in NA
mlp[mlp == ''] <- "Unknown"

# vars
num <- mlp$rownames
lat <- mlp$lat
long <- mlp$long

# column removal
police <- subset(mlp, select = -c(num,lat,long))

# vars
ID <- police$idNum
date <- as.Date(police$date)
problem <- as.numeric(police$problem)
mdc <- as.numeric(police$MDC)
citeIssue <- as.numeric(police$citationIssued)
personSearch <- as.numeric(police$personSearch)
vehicleSearch <- as.numeric(police$vehicleSearch)
preRace <- as.numeric(police$preRace)
gender <- as.numeric(police$gender)
precinct <- police$policePrecinct
neighbor <- as.numeric(police$neighborhood)
