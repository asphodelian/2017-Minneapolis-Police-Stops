#############
# Libraries #
#############

library(corrr)
library(caTools)
library(datasets)
library(dplyr)
library(factoextra)
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

#######
# PCA #
#######

# normalizing
numPol$month <- as.numeric(numPol$month)
normPol <- scale(numPol)

polCA <- princomp(normPol)
summary(polCA)
polCA$loadings 

# visualize
fviz_eig(polCA, addlabels = TRUE)
fviz_pca_var(polCA, col.var = "mediumslateblue")
fviz_cos2(polCA, choice = "var", axes = 1:2)
fviz_pca_var(polCA, col.var = "cos2",
             gradient.cols = c("mediumslateblue", "orange", "green"),
             repel = TRUE)

# prcomp
result <- prcomp(numPol, scale. = TRUE)
var <- result$sdev^2
ratio <- var/sum(var)
scree <- data.frame(
  Principal_Component = 1:length(ratio),
  Variance_Explained = ratio
)

ggplot(scree, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_line(group = 1) +
  geom_point() +
  scale_x_continuous(breaks = 1:nrow(scree)) +
  labs(
    title = "Scree Plot",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()

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
  

