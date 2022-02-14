## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(tidyverse)
library(psych)
library(ggplot2)
library(patchwork)
library(dplyr) 
library(caret) 
library(reshape2)
library(gridExtra)
library(ggcorrplot)


## ----------------------------------------------------------------------------------------------------------------------------------
# Download and load the data set

URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
liver <- read_csv(URL, col_names = FALSE)





## ----------------------------------------------------------------------------------------------------------------------------------
# name the data columns
colnames(liver) <- c("Age", "Gender", "totalBilirubin", "directBilirubin", "totalProteins", "Albumin"
                     ,"AGratio", "SGPT", "SGOT", "Alkphos", "Disease")


## ----------------------------------------------------------------------------------------------------------------------------------
# view the structure of the data set
str(liver)


## ----------------------------------------------------------------------------------------------------------------------------------
# describe the data set variables.
describe(liver)



## ----------------------------------------------------------------------------------------------------------------------------------
# check for missing values
liver[rowSums(is.na(liver))!= 0,]

# remove column which has missing values
liver <- na.omit(liver)


## ----------------------------------------------------------------------------------------------------------------------------------
# convert gender to numeric factor
liver$Gender[liver$Gender=='Male']<- 1
liver$Gender[liver$Gender=='Female']<- 2
liver$Gender <- as.factor(liver$Gender)


## ----------------------------------------------------------------------------------------------------------------------------------
# convert predictor variable to binary
liver$Disease[liver$Disease==2]<- 0
liver$Disease <- as.factor(liver$Disease)



## ----------------------------------------------------------------------------------------------------------------------------------
ph1 <- liver %>% ggplot(aes(x=Age)) +
    geom_histogram(binwidth=5)+
  labs(title = "Age", y = "Count")


ph2 <- liver %>% ggplot(aes(x=totalBilirubin)) +
    geom_histogram()+
  labs(title = "totalBilirubin", y = "Count")


ph3 <- liver %>% ggplot(aes(x=directBilirubin)) +
    geom_histogram()+
  labs(title = "directBilirubin", y = "Count")


ph4 <- liver %>% ggplot(aes(x=SGOT)) +
    geom_histogram()+
  labs(title = "SGOT", y = "Count")


ph5 <- liver %>% ggplot(aes(x=SGPT)) +
    geom_histogram()+
  labs(title = "SGPT", y = "Count")


ph6 <- liver %>% ggplot(aes(x=totalProteins)) +
    geom_histogram()+
  labs(title = "totalProteins", y = "Count")


ph7 <- liver %>% ggplot(aes(x=Albumin)) +
    geom_histogram()+
  labs(title = "Albumin", y = "Count")


ph8 <- liver %>% ggplot(aes(x=AGratio)) +
    geom_histogram()+
  labs(title = "AGratio", y = "Count")

ph9 <- liver %>% ggplot(aes(x=Alkphos)) +
    geom_histogram()+
  labs(title = "Alkphos", y = "Count")




## ----------------------------------------------------------------------------------------------------------------------------------
(ph1|ph2|ph3)/(ph4|ph5|ph6)/(ph7|ph8|ph9)


## ----echo=TRUE---------------------------------------------------------------------------------------------------------------------
pb1 <- liver %>% ggplot(aes(x= as.factor(Disease), y=Age)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb2 <- liver %>% ggplot(aes(x= as.factor(Disease), y=totalBilirubin)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb3 <- liver %>% ggplot(aes(x= as.factor(Disease), y=directBilirubin)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb4 <- liver %>% ggplot(aes(x= as.factor(Disease), y=SGOT)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb5 <- liver %>% ggplot(aes(x= as.factor(Disease), y=SGPT)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb6 <- liver %>% ggplot(aes(x= as.factor(Disease), y=totalProteins)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb7 <- liver %>% ggplot(aes(x= as.factor(Disease), y=Albumin)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb8 <- liver %>% ggplot(aes(x= as.factor(Disease), y=AGratio)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")

pb9 <- liver %>% ggplot(aes(x= as.factor(Disease), y=Alkphos)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Disease")


(pb1|pb2|pb3)/(pb4|pb5|pb6)/(pb7|pb8|pb9)


## ----eval=FALSE, include=FALSE-----------------------------------------------------------------------------------------------------
## p1 <- liver %>%
##   ggplot(aes(Gender, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Gender", y = "Number of Patients")
## 
## p2 <- liver %>%
##   ggplot(aes(Age, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Age", y = "Number of Patients")
## 
## p3 <- liver %>%
##   ggplot(aes(totalBilirubin, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Total Bilirubin", y = "Number of Patients")
## 
## p4 <- liver %>%
##   ggplot(aes(directBilirubin, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Direct Bilirubin", y = "Number of Patients")
## 
## p5 <- liver %>%
##   ggplot(aes(SGOT, fill = Disease)) +
##   geom_bar() +
##   labs(title = "SGOT", y = "Number of Patients")
## 
## p6 <- liver %>%
##   ggplot(aes(SGPT, fill = Disease)) +
##   geom_bar() +
##   labs(title = "SGPT", y = "Number of Patients")
## 
## p7 <- liver %>%
##   ggplot(aes(totalProteins, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Total protines", y = "Number of Patients")
## 
## p8 <- liver %>%
##   ggplot(aes(Albumin, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Albumin", y = "Number of Patients")
## 
## p9 <- liver %>%
##   ggplot(aes(AGratio, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Albumin Globumin ratio", y = "Number of Patients")
## 
## p10 <- liver %>%
##   ggplot(aes(Alkphos, fill = Disease)) +
##   geom_bar() +
##   labs(title = "Alkaline phostate", y = "Number of Patients")
## 


## ----eval=FALSE, include=FALSE-----------------------------------------------------------------------------------------------------
## (p1|p2)
## (p3 |p4)
## (p5|p6)
## (p7|p8)
## (p9| p10)
## 


## ----------------------------------------------------------------------------------------------------------------------------------
# calculate the correlations of variables without Gender and Disease Status
correlations <- cor(liver[,c(-2,-11)])
correlations



## ----------------------------------------------------------------------------------------------------------------------------------
ggcorrplot(correlations, hc.order = TRUE, type = "lower",  lab = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------
# find highly correlated variables
high_coor_cols <- findCorrelation(correlations, cutoff = 0.7, names = TRUE)
high_coor_cols

# remove highly correlated data
liver_data <- liver[, !names(liver) %in% high_coor_cols]
dim(liver_data)


## ----------------------------------------------------------------------------------------------------------------------------------
# set a seed
set.seed(2021)
# split data set into train and test
index <- createDataPartition(liver_data$Disease, p = 0.8, list = FALSE)
train = liver_data[index,]
test = liver_data[-index,]
dim(train)
dim(test)


## ----------------------------------------------------------------------------------------------------------------------------------
# Logistic Regression - GLM
# set seed
set.seed(2021)
# fit/train the model
model1_glm_fit <- train(Disease ~., data = train, method = "glm", family = "binomial")

# predict outcomes on test
model1_glm_pred <- predict(model1_glm_fit, test)
# confusion matrix
model1_cm <- confusionMatrix(model1_glm_pred, test$Disease)
model1_cm
# model accuracy
model1_acc <- model1_cm$overall["Accuracy"]
model1_sen <- model1_cm$byClass["Sensitivity"]
model1_spe <- model1_cm$byClass["Specificity"]
model1_pre <- model1_cm$byClass["Precision"]
model1_acc


## ----------------------------------------------------------------------------------------------------------------------------------
# knn
# set seed
set.seed(2021)
# fit/train the model
model2_knn_fit <- train(Disease ~ ., data = train, method = "knn")

# predict outcomes on test
model2_knn_pred <- predict(model2_knn_fit, test)
# confusion matrix
model2_cm <- confusionMatrix(model2_knn_pred, test$Disease)
model2_cm
# model accuracy
model2_acc <- model2_cm$overall["Accuracy"]
model2_sen <- model2_cm$byClass["Sensitivity"]
model2_spe <- model2_cm$byClass["Specificity"]
model2_pre <- model2_cm$byClass["Precision"]
model2_acc


## ----------------------------------------------------------------------------------------------------------------------------------
# Random Forest
# set seed
set.seed(2021)
# fit/train the model
model3_rf = train(Disease ~ ., method = "rf", data = train, prox = TRUE)

# predict outcomes on test
model3_pred <- predict(model3_rf, test)
# confusion matrix
model3_cm <- confusionMatrix(model3_pred, test$Disease)
model3_cm
# model accuracy
model3_acc <- model3_cm$overall["Accuracy"]
model3_sen <- model3_cm$byClass["Sensitivity"]
model3_spe <- model3_cm$byClass["Specificity"]
model3_pre <- model3_cm$byClass["Precision"]
model3_acc




## ----------------------------------------------------------------------------------------------------------------------------------
# Naive Bayes
# set seed
set.seed(2021)
# fit/train the model
model4_nb_fit <- train(Disease ~ ., data = train, method = "nb")


# predict outcomes on test
model4_nb_pred <- predict(model4_nb_fit, test)
# confusion matrix
model4_cm <- confusionMatrix(model4_nb_pred, test$Disease)
model4_cm
# model accuracy
model4_acc <- model4_cm$overall["Accuracy"]
model4_sen <- model4_cm$byClass["Sensitivity"]
model4_spe <- model4_cm$byClass["Specificity"]
model4_pre <- model4_cm$byClass["Precision"]
model4_acc


## ----------------------------------------------------------------------------------------------------------------------------------
# gamLoess
# set seed
set.seed(2021)
# fit/train the model
model5_nb_fit <- train(Disease ~ ., data = train, method = "gamLoess")
#model5_nb_fit

# predict outcomes on test
model5_nb_pred <- predict(model5_nb_fit, test)
# confusion matrix
model5_cm <- confusionMatrix(model5_nb_pred, test$Disease)
model5_cm
# model accuracy
model5_acc <- model5_cm$overall["Accuracy"]
model5_sen <- model5_cm$byClass["Sensitivity"]
model5_spe <- model5_cm$byClass["Specificity"]
model5_pre <- model5_cm$byClass["Precision"]
model5_acc


## ----------------------------------------------------------------------------------------------------------------------------------
# Overview accuracy
# create table with accuracies overview
tibble(method = c("glm", "knn", "random forest","naive bayes","gamLoess"),
       Accuracy = c(model1_acc, model2_acc, model3_acc, model4_acc, model5_acc),
       Sensitivity = c(model1_sen, model2_sen, model3_sen, model4_sen, model5_sen),
       Specificity = c(model1_spe, model2_spe, model3_spe, model4_spe, model5_spe),
       Precision = c(model1_pre, model2_pre, model3_pre, model4_pre, model5_pre))


## ----echo=TRUE, results='hide'-----------------------------------------------------------------------------------------------------
# Ensembler
# list of models
models <- c("glm", "naive_bayes",  "svmLinear", "gamLoess",
            "knn","rf", "avNNet", "nnet" )

# set seed
set.seed(2021)
# fit/train models
fits <- lapply(models, function(model){ 
  # print(model)
  train(Disease ~ ., method = model, data = train)
}) 

# predict with each model
fits_predicts <- sapply(fits, function(fits){
  predict(fits, test) 
})

# calculate accuracies for each model
acc <- colMeans(fits_predicts == test$Disease)
#acc

# obtain mean score of predictions
votes <- rowMeans(fits_predicts == 1)
# accumulated voting, below mean of 0.5 vote is 0
y_hat <- ifelse(votes > 0.5, 1, 0)
# ensembler accuracy mean is mean of accumulated votes accuracy
ensembler_mean <- mean(y_hat == test$Disease)


# which individual models are better than the ensembler mean
better <- ensembler_mean < acc



## ----------------------------------------------------------------------------------------------------------------------------------

ensembler_mean
acc[which(better==T)]

