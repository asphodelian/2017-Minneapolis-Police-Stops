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
                     day = format(date, "%d"), 
                     month = format(date, "%m"), 
                     year = format(date, "%Y"))

polDate <- subset(policeDate, select = -c(year, date))
# problem into 1,0
policeDate$problem <- ifelse(polDate$problem == "traffic",1,0)


# removing columns for PCA
numPol <- subset(policeDate, select = -c(X, idNum, preRace, race, date, year, neighborhood, gender))

#######
# PCA #
#######

# normalizing
numPol$month <- as.numeric(numPol$month)
numPol$day <- as.numeric(numPol$day)
normPol <- scale(numPol)

# PCA
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
summary(result)
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

######################
# K-Means Clustering #
######################

set.seed(1)

# silhoulette
fviz_nbclust(normPol, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

# testing the clusters
k2 <- kmeans(normPol, centers = 2, nstart = 25)
k4 <- kmeans(normPol, centers = 4, nstart = 25)
k10 <- kmeans(normPol, centers = 10, nstart = 25)

# Compare cluster centers and sizes
print(k2$centers)
print(k4$centers)
print(k10$centers)

# Visualize clustering solutions
fviz_cluster(k2, data = normPol) + ggtitle("K = 2")
fviz_cluster(k4, data = normPol) + ggtitle("K = 4")
fviz_cluster(k10, data = normPol) + ggtitle("K = 10")

##############################
# Splitting Data: Train/Test #
##############################

polsample <- sample.split(polDate, SplitRatio = 0.8)
train <- subset(polDate, polsample == TRUE)
test <- subset(polDate, polsample == FALSE)
