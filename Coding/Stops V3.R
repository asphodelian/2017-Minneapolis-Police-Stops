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
library(pROC)
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


# removing columns for PCA
numPol <- subset(policeDate, select = -c(X, idNum, preRace, race, date, year, neighborhood, gender, citationIssued))

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
#k4 <- kmeans(normPol, centers = 4, nstart = 25)
#k10 <- kmeans(normPol, centers = 10, nstart = 25)

# Compare cluster centers and sizes
print(k2$centers)
#print(k4$centers)
#print(k10$centers)

# Visualize clustering solutions
fviz_cluster(k2, data = normPol) + ggtitle("K = 2")
#fviz_cluster(k4, data = normPol) + ggtitle("K = 4")
#fviz_cluster(k10, data = normPol) + ggtitle("K = 10")

# View first few cluster assignments
head(k2$cluster)
# Combine the cluster assignments with the original data
data_with_clusters <- cbind(normPol, Cluster = k2$cluster)

# View the first few rows of the new data with clusters
head(data_with_clusters)

# Calculate the mean of each variable within each cluster
aggregate(normPol, by = list(Cluster = k2$cluster), FUN = mean)

# Example using the first two dimensions of normPol
ggplot(data_with_clusters, aes(x = normPol[, 1], y = normPol[, 2], color = factor(Cluster))) +
  geom_point() +
  labs(title = "K-means Clustering", x = "Variable 1", y = "Variable 2") +
  theme_minimal()

# cluster visual
cluster_means <- aggregate(cbind(personSearch, vehicleSearch) ~ Cluster, 
                           data = data.frame(normPol, Cluster = k2$cluster), FUN = mean)

# Reshape for ggplot
cluster_long <- pivot_longer(cluster_means, 
                             cols = c(personSearch, vehicleSearch), 
                             names_to = "SearchType", 
                             values_to = "MeanValue")

# Create a bar plot
ggplot(cluster_long, aes(x = factor(Cluster), y = MeanValue, fill = SearchType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Means by Search Type", x = "Cluster", y = "Mean Value") +
  scale_fill_manual(values = c("personSearch" = "blue", "vehicleSearch" = "red")) +
  theme_minimal()

#######################
# Supervised Learning #
#######################

upDate <- newDate[,-c(1,2,12,14)]

# splitting data
polsample <- sample.split(upDate, SplitRatio = 0.8)
train <- subset(upDate, polsample == TRUE)
test <- subset(upDate, polsample == FALSE)

#################
# Decision Tree #
#################

# model 1
model1 <- rpart(citationIssued ~ ., train)
rpart.plot(model1)
summary(model1)
# Predictions and confusion matrix
predictions <- predict(model1, newdata = test, type = "class")
confusionMatrix <- table(Predicted = predictions, Actual = test$citationIssued)
print(confusionMatrix)

# model 2
model2 <- rpart(citationIssued~., train, control = rpart.control(cp = 0.01))
rpart.plot(model2, type = 4)  # Detailed plot
summary(model2)
# Predictions and confusion matrix
predictions <- predict(model2, newdata = test, type = "class")
confusionMatrix <- table(Predicted = predictions, Actual = test$citationIssued)
print(confusionMatrix)

#########
# LASSO #
#########

# Prepare predictor matrix and response variable for training data
trainX <- model.matrix(citationIssued ~ . - 1, data = train)  # Convert to dummy variable matrix
train$citationIssued <- ifelse(train$citationIssued == "Yes", 1, 0)
trainY <- as.numeric(as.character(train$citationIssued))  # Convert factor to numeric (response variable)

# Perform cross-validation to find optimal lambda
cvMod <- cv.glmnet(trainX, trainY, alpha = 1, family = "binomial", nfolds = 5)
bestLam <- cvMod$lambda.min  # Optimal lambda value

# Train the final LASSO model using the optimal lambda
finalMod <- glmnet(trainX, trainY, alpha = 1, lambda = bestLam, family = "binomial")

# Prepare predictor matrix for the test data
testX <- model.matrix(citationIssued ~ . - 1, data = test, contrasts.arg = attr(trainX, "contrasts"))  # Match training structure

# Predict probabilities on the test data
predictions <- predict(finalMod, testX, type = "response")

# Convert probabilities into binary classes using a threshold of 0.5
classPredict <- ifelse(predictions > 0.5, 1, 0)

# Evaluate performance with a confusion matrix
conf_matrix <- table(classPredict, test$citationIssued)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# Plot cross-validation error
plot(cvMod, main = "Cross-Validation Error vs Lambda")

# Highlight the optimal lambda
abline(v = log(cvMod$lambda.min), col = "mediumslateblue", lty = 2, lwd = 3)
legend("topright", legend = paste("Optimal Lambda:", round(cvMod$lambda.min, 4)), col = "mediumslateblue", lty = 2)

# Plot coefficient paths
plot(finalMod, xvar = "lambda", label = TRUE, main = "LASSO Coefficient Paths")

# Add a vertical line at the optimal lambda
abline(v = log(bestLam), col = "blue", lty = 2, lwd = 2)
legend("topright", legend = paste("Optimal Lambda:", round(bestLam, 4)), col = "blue", lty = 2)

# Calculate the ROC curve
predict <- as.numeric(predictions)
roc_curve <- roc(test$citationIssued, predict)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for LASSO Model", col = "deeppink3", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Add AUC value
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC:", round(auc_value, 4)), col = "deeppink3", lwd = 2)

# Visualize the distribution of probabilities
hist(predictions, main = "Predicted Probabilities", xlab = "Probability", col = "goldenrod2")

