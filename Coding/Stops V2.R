#############
# Libraries #
#############

library(dplyr)
library(ggplot2)
library(lubridate)
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

######################
# Splitting the Date #
######################

police$date <- as.Date(police$date)
transform(police, date = format(date, "%d"), 
          month = format(date, "%m"), year = format(date, "%Y"))
