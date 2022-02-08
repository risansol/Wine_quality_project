### wine qualitiy ###
## P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine 
## preferences by data mining from physicochemical properties. 
## In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
## https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009/version/2

### libraries used in the project

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(corrplot)) install.packages("corrplot")
if(!require(class)) install.packages("class")
if(!require(gmodels)) install.packages("gmodels")
if(!require(dplyr)) install.packages("dplyr")
if(!require(scales)) install.packages("scales")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(randomForest)) install.packages("randomForest")

library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(ggplot2)
library(corrplot)
library(class)
library(gmodels)
library(dplyr)
library(scales)
library(rpart)
library(rpart.plot) 
library(randomForest)

## loading data
wine_data <- read.csv("~/Documents/Curso_DataSience(edX)/9-Capstone Project/My own project/wine_quality_project/winequality-red.csv")

# any NA value to clean 
any(is.na(wine_data))
# [1] FALSE  ==> (no NA in the dataset)

# a first view of the data
str(wine_data)
summary(wine_data [-12])

##=====
## data analysis##
# to explore and visualize the data
##=====

# quality distribution
# histogram to determine the quality distribution

wine_data %>% 
  ggplot(aes(x = quality)) +
  scale_x_continuous(breaks = seq(3,8,1))+
  geom_bar(fill = "dark grey", color = "black", alpha = 0.8)+
  labs(title ="Quality distribution", x = "Quality")

# density plot

wine_data %>% 
  ggplot(aes(x = quality)) +
  scale_x_continuous(breaks = seq(3,8,1))+
  geom_density(fill = "dark grey", color = "black", alpha = 0.8)+
  labs(title ="Quality distribution", x = "Quality")


# histograms to know the distribution of the other characteristics
#remove "quality" column, nº12, because it has been already graphed 


wine_data[-12]%>%
  gather(Attributes, value, 1:11) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Wines Attributes (Histogram)") 

#density plot for each attribute. Again, we remove "quality"

wine_data[-12]%>%
gather(Attributes, value, 1:11) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="dark grey", alpha=0.6, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free") +
  labs(x="Values", y="Density",
       title="Wines Attributes (Density plots)") 

# box-plot for each attribute (quality variable never included)

# two different plots: 
  # one without "free.sulfur.dioxide"  and "total.sulfur.dioxide", cols 6  and 7
      #their values are so high that worsens the visualization.
  # one with the others variables

summary(wine_data[c(6,7)])

#box-plot one
wine_data[-c(6,7,12)]%>%
  gather(Attributes, value, 1:9) %>% 
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Wines Attributes (Boxplots)") +
    theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_flip()
  
# box-plot two

wine_data[c(6,7)]%>%
  gather(Attributes, value, 1:2) %>% 
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Wines Attributes (Boxplots) II") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  coord_flip()

 
# correlation matrix

corrplot(cor(wine_data), type="lower", method="number", number.cex = 0.65,
         number.font= 2, tl.cex=0.8)


## Accordingly to the correlation matrix, alcohol and volatile acidity are 
# the attributes with the most relevant relationship to quality.

##box plot quality / alcohol

wine_data %>% 
  ggplot(aes(x = factor (quality), y= alcohol))+
  geom_jitter( alpha = .2) +
  geom_boxplot(show.legend=FALSE)+
  labs(title = "Alcohol vs Quality ", x = "Quality", 
       y = "Alcohol (% by volume)")


##box plot quality / volatil.acidity

wine_data %>% 
  ggplot(aes(x = factor (quality), y= volatile.acidity))+
  geom_jitter( alpha = .2) +
  geom_boxplot(show.legend=FALSE)+
  labs(title = "Volatile acidity vs Quality ", x = "Quality", 
       y = "Volatile acidity (g/dm3)")

##box plot quality / sulphates

wine_data %>% 
  ggplot(aes(x = factor (quality), y= sulphates))+
  geom_jitter( alpha = .2) +
  geom_boxplot(show.legend=FALSE)+
  labs(title = "Sulphates vs Quality ", x = "Quality", 
       y = "Sulphates (g/dm3)")


#as alcohol is highly correlated with quality (0,48), and density is
# highly correlated with alcohol (-0,5), we can plot this relation also

wine_data %>% 
  ggplot(aes(x=density, y = alcohol))+
  geom_jitter( alpha = .2) +
  geom_smooth()+
  labs(title = "Alcohol vs Density ", x ="Density (mg/dm3)", 
       y ="Alcohol (% by volume)")

#don't forget that density is also correlated with residual sugar

wine_data %>% 
  ggplot(aes(x=density, y = residual.sugar))+
  geom_jitter( alpha = .2) +
  geom_smooth()+
  labs(title = "Fixed acidity vs Density ", x ="Density (mg/dm3)", 
       y ="Residual sugar (g/dm3)")

#density is highly correlated with fixed.acidity (0,67) but we don't 
#include this plot in the report due to the very low correlation to quality
# we plot, instead, citric.acide vs volatile (corr = -0,55), because volatile is
# correlated to quality (-0,3)


wine_data %>% 
  ggplot(aes(x=citric.acid, y = volatile.acidity))+
  geom_jitter( alpha = .2) +
  geom_smooth()+
  labs(title = "Citric acid vs Volatile acidity ", x ="Volatile acidity (g/dm3)", 
       y ="Citric acid (g/dm3)")



####
### MODELING ####
####

###  normalize data to avoid that columns with higher values cause 
##  inconsistencies in the analysis


## following calculation are not included in the final report: jut to 
# test that normalize variables have the same relation amgong them than in 
# the original dataset

##I normalize forcing new data will be a dataframe!!

wine_data_Norm <- as.data.frame(scale(wine_data, center = TRUE, scale = TRUE))
str(wine_data_Norm)

##compruebo relaciones entre un par de variables para ver que son igual
# en base datos original y en la normalizada

#original
g1 <- wine_data %>% 
  ggplot(aes(x = density, y = alcohol))+
  geom_point()+
  labs(title = "Original database")
g1

#normalized

g2 <- wine_data_Norm %>% 
  ggplot(aes(x = density, y = alcohol))+
  geom_point()+
  labs(title = "Normalized database")
g2

## compruebo que las corr siguen siendo las mismas
corrplot(cor(wine_data_Norm), type="lower", method="number", number.cex = 0.65,
         number.font= 2, tl.cex=0.8)
## ok!! todo está bien!!



### NOTE: validation metrics RMSE, MAE, R2

##============
### we will define some function we may use to evaluate the different models
##============

##calculating Mean Absolute Error (MAE)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

##calculating Residual Mean Squared Error (RMSE)

RMSE <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

###  calculating R squared

R <- function(actual, predicted){
  (sum((predicted - mean(actual))^2))/(sum((actual - mean(actual))^2))
}


#the size of the data base let's make cross validation
#Create training and test set from observations

nrow(wine_data)
set.seed(123)
n <- nrow(wine_data)
trainIndex <- sample(1:n, size = round(0.7*n),replace=FALSE)

training <- wine_data[trainIndex,]
testing <- wine_data[-trainIndex,]



### different ML algorithms ####

##========
##=== MODEL 0 => linear regression 
##========

# standarize the data

preProcValues <- preProcess(wine_data, method = c("center", "scale"))
wine_data_sc <- predict(preProcValues, wine_data)

training_sc <- wine_data_sc[trainIndex,]
testing_sc <- wine_data_sc[-trainIndex,]


##with complete dataset ______________________________________________________

#train the model
lm_wine <- lm(quality ~., data = training_sc)

summary(lm_wine)

#predict

lm_wine_hat <- predict(lm_wine, testing_sc)

#summary statistics of the quality predicted #
summary(lm_wine_hat)

#summary statistics of the real quality#
summary(testing_sc$quality)

## model valuation 

mae_lm<-MAE(predicted = lm_wine_hat, actual = testing_sc$quality)
rmse_lm<-RMSE(predicted = lm_wine_hat, actual = testing_sc$quality)
r2_lm<-R(predicted = lm_wine_hat, actual = testing_sc$quality)

data.frame(metric = c("RMSE","MAE","R_2"),
           value = round(c(mae_lm,rmse_lm,r2_lm),2))


##========
##=== MODEL 1 => K-NN 
##========


#repeat; 10-fold cross validation
ctrl<-trainControl(method = "cv", number = 5, classProbs = TRUE)

#Train model

knn_wine <- train(quality~., data = training,
                  method = "knn",
                  preProcess= "center",  ## we should normalize the data (explanation in the KNN section in report)
                  trControl = ctrl,
                  metric="RMSE",
                  tuneLength = 5,
                  tuneGrid = data.frame(k= seq(1,100,1)))

knn_wine$results
ggplot(knn_wine, highlight = TRUE)


##determine the k value that provides the best tune
knn_wine$bestTune

##now, we use this k to validate the test set

knn_wine_hat <- knn(train=training, test=testing,
                    cl = training$quality,k= knn_wine$bestTune)

### calculating accuracy

confusionMatrix(as.factor(knn_wine_hat),
                as.factor(testing$quality))$overall["Accuracy"]


### calculating table

confusionMatrix(as.factor(knn_wine_hat),
                as.factor(testing$quality))$table

#summary statistics of the quality predicted #
summary(knn_wine_hat)

#summary statistics of the real quality#
summary(testing$quality)

## proxy or the validation metrics

mae_knn <-mean(knn_wine$results$MAE)
rmse_knn <- mean(knn_wine$results$RMSE)
r2_knn<-mean(knn_wine$results$Rsquared)

data.frame(metric = c("RMSE","MAE","R_2"),
           value = round(c(mae_knn,rmse_knn,r2_knn),2))


##========
#== MODEL 2 => REGRESSIONN TREE
##========

# The "rpart" package trains regression trees  

# Train the model

tree_wine <- rpart(quality ~ ., data = training)
tree_wine
summary(tree_wine)
rpart.plot(tree_wine, digits = 2, fallen.leaves = TRUE)

#predict

tree_wine_hat <- predict(tree_wine, testing)

table(pred= round(tree_wine_hat,1),actual=testing$quality)

#summary statistics of the quality predicted #
summary(tree_wine_hat)

#summary statistics of the real quality#
summary(testing$quality)


## model valuation 

mae_tree<-MAE(predicted = tree_wine_hat, actual = testing$quality)
rmse_tree<-RMSE(predicted = tree_wine_hat, actual = testing$quality)
r2_tree<-R(predicted = tree_wine_hat, actual = testing$quality)

data.frame(metric = c("RMSE","MAE","R_2"),
           value = round(c(mae_tree,rmse_tree,r2_tree),2))

##========
#== MODEL 3 => RANDOM FOREST
##========


### with function
rf_wine <- randomForest(quality ~ ., data=training)
rf_wine

#Random forest error curve

plot(rf_wine, main = "Random forest error curve")

rf_wine_hat <- predict(rf_wine, testing )
table(pred= round(rf_wine_hat,1) ,actual=testing$quality)

#summary statistics of the quality predicted #
summary(rf_wine_hat)

#summary statistics of the real quality#
summary(testing$quality)

# Get importance variable

varImp(rf_wine)
varImpPlot(rf_wine,type=2, main = "Random Forest: Variable importante")


## model valuation 

mae_rf <-MAE(predicted = rf_wine_hat, actual = testing$quality)
rmse_rf<- RMSE(predicted = rf_wine_hat, actual = testing$quality)
r2_rf <- R(predicted = rf_wine_hat, actual = testing$quality)


data.frame(metric = c("RMSE","MAE","R_2"),
           value = round(c(mae_rf,rmse_rf,r2_rf),2))

##---
### comparative  matrix with all the previous results
##---

valuation_result <- data.frame(model  = c("Linear Regression", "Knn", "Regression Tree", "Random Forest"), 
                               RMSE = round (c(rmse_lm, rmse_knn,rmse_tree,rmse_rf),2),
                               MAE = round (c(mae_lm, mae_knn,mae_tree,mae_rf),2),
                               R_2= round (c(r2_lm, r2_knn,r2_tree,r2_rf),2))

valuation_result

knitr::kable(valuation_result)


### the end ###


### annex

#wine estructure by quality
wine_data %>% 
  group_by(quality) %>% 
  count() 

#number of quality==5
sum(with(wine_data,quality == "5"))

#number of quality==6
sum(with(wine_data,quality == "6"))


#proportion quality 5&6 in total

round(100*((sum(with(wine_data,quality == "5")) + sum(with(wine_data,quality == "5")))/ length(wine_data$quality)),1)

  

