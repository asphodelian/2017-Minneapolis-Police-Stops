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
mdc <- mlp$MDC

# column removal
police <- subset(mlp, select = -c(mdc,num,lat,long))

# yes/no --> 1/0
police$citationIssued <- ifelse(police$citationIssued == "YES",1,0)
police$personSearch <- ifelse(police$personSearch == "YES",1,0)
police$vehicleSearch <- ifelse(police$vehicleSearch == "YES",1,0)

# counts
table(as.Date(police$date))
table(police$problem)
table(police$citationIssued)
table(police$personSearch)
table(police$vehicleSearch)
table(police$preRace)
table(police$race)
table(police$gender)
table(police$policePrecinct)
table(police$neighborhood)
