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

########################
# Bootstrap Validation #
########################

set.seed(1)

# Define a function for K-Means clustering stability
bootstrap_clustering <- function(data, k, n_iter = 100) {
  cluster_consistency <- numeric(n_iter)
  
  for (i in 1:n_iter) {
    # Bootstrap resample the data
    resample_indices <- sample(1:nrow(data), replace = TRUE)
    resampled_data <- data[resample_indices, ]
    
    # Perform K-Means on original and resampled data
    original_clusters <- kmeans(data, centers = k, nstart = 25)$cluster
    resampled_clusters <- kmeans(resampled_data, centers = k, nstart = 25)$cluster
    
    # Match clusters using adjusted Rand index (ARI)
    cluster_consistency[i] <- cluster.stats(dist(data), original_clusters, resampled_clusters)$corrected.rand
  }
  
  return(cluster_consistency)
}

# Apply the function
k <- 4
stability <- bootstrap_clustering(normPol, k)

# Summary of stability
summary(stability)
hist(stability, main = "Bootstrap Cluster Stability (ARI)", xlab = "Adjusted Rand Index")

####################
# Cross-Validation #
####################

set.seed(1)

cross_validate_clustering <- function(data, k, n_folds = 5) {
  folds <- sample(rep(1:n_folds, length.out = nrow(data)))
  consistency <- numeric(n_folds)
  
  for (fold in 1:n_folds) {
    # Split into training and testing sets
    train_data <- data[folds != fold, ]
    test_data <- data[folds == fold, ]
    
    # Train K-Means on the training set
    kmeans_train <- kmeans(train_data, centers = k, nstart = 25)
    
    # Predict clusters for the test set
    nearest_centers <- apply(test_data, 1, function(x) {
      which.min(colSums((kmeans_train$centers - x)^2)) # Assign to closest center
    })
    
    # Combine train and test cluster labels into a single vector
    full_clustering <- integer(nrow(data))
    full_clustering[folds != fold] <- kmeans_train$cluster  # Train clusters
    full_clustering[folds == fold] <- nearest_centers       # Test clusters
    
    # Compute Adjusted Rand Index comparing clusters on the entire dataset
    original_clusters <- kmeans(data, centers = k, nstart = 25)$cluster
    consistency[fold] <- cluster.stats(dist(data), original_clusters, full_clustering)$corrected.rand
  }
  
  return(consistency)
}

# Apply the function
k <- 2
cv2 <- cross_validate_clustering(normPol, k)

# Summary of cross-validation stability
summary(cv2)
hist(cv2, main = "Cross-Validation Cluster Stability (ARI)", xlab = "Adjusted Rand Index")

# testing across k-values
k <- 4
cv4 <- cross_validate_clustering(normPol, k)
summary(cv4)
hist(cv4, main = "Cross-Validation Cluster Stability (ARI)", xlab = "Adjusted Rand Index")

# testing across k-values
k <- 10
cv10 <- cross_validate_clustering(normPol, k)
summary(cv10)
hist(cv10, main = "Cross-Validation Cluster Stability (ARI)", xlab = "Adjusted Rand Index")


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


  

