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


