rm(list = ls())
install.packages("caret",dependencies = TRUE,INSTALL_opts = "--no-lock")
install.packages("car")
#1.
library(ggplot2)
library(lattice)
library(caret)
library(carData)
library(car)
library("ade4")
library(tidyverse)
#reading env and fish data, and summarizing fish abundance data by sites. 
data(doubs, package = "ade4")
env <- doubs$env
fish <-doubs$fish
fish_sum <- apply(fish, 1, sum)
fish_df <- data.frame(Site = rownames(fish), Fish_Abundance = fish_sum)
#combining env and total fish to a new data frame named as "env_fish".
env_fish <- cbind(env, fish_df[match(rownames(env), fish_df$Site), 2])
#2.
#visualizing the features of the new env_fish set using scatterplot().
names(env_fish)[12] <- "Fish_Abundance"
x_vars <- c("dfs", "alt", "slo", "flo", "pH", "har", "pho", "nit", "amm", "oxy", "bdo")
y_var <- "Fish_Abundance"
x_data <- env_fish[, x_vars]
y_data <- env_fish[[y_var]]
scatterplotMatrix(x_data, diagonal = list(method = "boxplot"), smooth = TRUE, 
                  regLine = TRUE, legend = TRUE,
                  var.labels = c("\n\n\nDFS", "\n\n\nALT", "\n\n\nSLO", "\n\n\nFLO", "\n\n\npH", 
                                 "\n\n\nHAR", "\n\n\nPHO", "\n\n\nNIT", "\n\n\nAMM", "\n\n\nOXY", "\n\n\nBDO"),
                  cex = 1, cex.labels = 1.5, cex.axis = 1.5,
                  pch = c(16, 16, 16), col = c("red", "green3", "blue"), row1attop = TRUE)
featurePlot(x=env_fish[, -12],
            y=env_fish[, 12],
            plot = "scatter",
            type=c("p","smooth"),
            layout=c(3,4))
#checking linear relationships between environmental variables and the total fish abundance at the sites
cor(env_fish[, 1:11], env_fish[, 12], method = "pearson")
#3.
#delete sites which have no fishes.
env_fish <- env_fish[rowSums(fish) > 0, ]
#removing all rows where any column contains an outlier.
env_fish <- env_fish[complete.cases(env_fish), ]
#4.
#identifying near zero-variance, outlies of the env variables.
#and excluding them for analysis.
nearZeroVar(env_fish,name=T,saveMetrics= TRUE)
#5.
#detecting the collinearity among env variables or removing highly correlated features 
#(with an absolute correlation of 0.75 or higher)
cor.env <- cor(env_fish[, 1:11])
highlyCor <- findCorrelation(cor.env, cutoff = 0.75)
env_fish <- env_fish[, -highlyCor]
#scaling and centering the env variables.
env_fish[, 1:5] <- scale(env_fish[, 1:5], center = TRUE, scale = TRUE)
#6.
#Spliting data into training and testing datasets
#
set.seed(123)
trainIndex <- sample(1:nrow(env_fish), 0.7*nrow(env_fish))
train <- env_fish[trainIndex, ]
test <- env_fish[-trainIndex, ]
# Keeping response variables as continuous values
train$Fish_Abundance <- as.numeric(train$Fish_Abundance)
test$Fish_Abundance <- as.numeric(test$Fish_Abundance)

#Creating Control Parameter Objects
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 3, 
                     summaryFunction = defaultSummary)

#Training random forest regression model
rf_model <- train(Fish_Abundance ~ ., 
                  data = train, 
                  method = "rf",
                  metric = "RMSE",
                  trControl = ctrl)
#Predicting Test Data
#Outputing prediction results
predictions <- predict(rf_model, newdata = test)
print(predictions)
print(rf_model)
