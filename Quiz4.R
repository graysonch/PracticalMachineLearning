

library(AppliedPredictiveModeling)
library(dplyr)
library(Hmisc)
library(gridExtra)

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. 
# Fit 
# (1) a random forest predictor relating the factor variable y to the remaining variables and 
# (2) a boosted predictor using the "gbm" method. Fit these both with the train() command in the caret package. 
# 
# What are the accuracies for the two approaches on the test data set? What is the accuracy among 
# the test set samples where the two methods agree? 
Qn1 <- function() {
    
    library(ElemStatLearn)
    library(caret)
    data(vowel.train)
    data(vowel.test)
    vowel.test$y <- factor(vowel.test$y)
    vowel.train$y <- factor(vowel.train$y)
    set.seed(33833)
    #str(vowel.train)
    modelFit_rf <- train(y~.,data=vowel.train,method="rf")
    pred_rf <- predict(modelFit_rf,vowel.test)
    model_result_rf <- confusionMatrix(vowel.test$y,pred_rf)
    print(model_result_rf$overall["Accuracy"])
    
    modelFit_boost <- train(y~.,data=vowel.train,method="gbm",verbose=FALSE)
    pred_boost <- predict(modelFit_boost,vowel.test)
    model_result_boost <- confusionMatrix(vowel.test$y,pred_boost)
    print(model_result_boost$overall["Accuracy"])
    
    agreeIdx <- pred_rf == pred_boost
    model_result_comb <- confusionMatrix(vowel.test$y[agreeIdx],pred_rf[agreeIdx])
    print(model_result_comb$overall["Accuracy"])
}

Qn1()

# Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), 
# boosted trees ("gbm") and linear discriminant analysis ("lda") model. Stack the predictions together 
# using random forests ("rf"). What is the resulting accuracy on the test set? Is it better or worse 
# than each of the individual predictions? 

Qn2 <- function() {
    library(caret)
    library(gbm)
    set.seed(3433)
    library(AppliedPredictiveModeling)
    
    data(AlzheimerDisease)
    adData = data.frame(diagnosis,predictors)
    inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
    training = adData[ inTrain,]
    testing = adData[-inTrain,]
    
    set.seed(62433)
    
    modelFit_rf <- train(diagnosis~.,data=training,method="rf")
    pred_rf <- predict(modelFit_rf,testing)
    print(paste0("Random Forrest ",confusionMatrix(testing$diagnosis,pred_rf)$overall["Accuracy"]))
    
    modelFit_boost <- train(diagnosis~.,data=training,method="gbm",verbose=FALSE)
    pred_boost <- predict(modelFit_boost,testing)
    print(paste0("Boost ",confusionMatrix(testing$diagnosis,pred_boost)$overall["Accuracy"]))
    
    modelFit_lda <- train(diagnosis~.,data=training,method="lda")
    pred_lda <- predict(modelFit_lda,testing)
    print(paste0("LDA ",confusionMatrix(testing$diagnosis,pred_lda)$overall["Accuracy"]))
    
    predDF <- data.frame(pred_rf,pred_boost,pred_lda,diagnosis=testing$diagnosis)
    CombModFit <- train(diagnosis~.,method="rf",data=predDF)
    pred_comb <- predict(CombModFit,predDF)
    print(paste0("STACKED ",confusionMatrix(testing$diagnosis,pred_comb)$overall["Accuracy"]))
    
}

Qn2()

# Set the seed to 233 and fit a lasso model to predict Compressive Strength. Which variable is the 
# last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet). 
Qn3 <- function() {
    set.seed(3523)
    library(AppliedPredictiveModeling)
    data(concrete)
    inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
    training = concrete[ inTrain,]
    testing = concrete[-inTrain,]
    set.seed(3523)
    
    modelFit_lasso <- train(CompressiveStrength~.,data=training,method="lasso")
    library(elasticnet)
    plot.enet(modelFit_lasso$finalModel, xvar = "penalty", use.color = TRUE)
}

Qn3()

# Fit a model using the bats() function in the forecast package to the training time series. 
# Then forecast this model for the remaining time points. 
# For how many of the testing points is the true value within the 95% prediction interval bounds? 

Qn4 <- function() {
    library(lubridate) # For year() function below
    library(forecast)
    
    dat = read.csv("./data/gaData.csv")
    training = dat[year(dat$date) < 2012,]
    testing = dat[(year(dat$date)) > 2011,]
    tstrain = ts(training$visitsTumblr)
    
    fit <- bats(tstrain)
    fcast <- forecast(fit, nrow(testing), level = c(95))
    plot(fcast)
    points(dat$visitsTumblr)
    
    print(sum(fcast$lower <= testing$visitsTumblr & testing$visitsTumblr <= fcast$upper) / dim(testing)[1])
    
}

Qn4()

# Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive 
# Strength using the default settings. Predict on the testing set. What is the RMSE?

Qn5 <- function() {
    set.seed(3523)
    library(caret)
    library(gbm)
    library(AppliedPredictiveModeling)
    data(concrete)
    inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
    training = concrete[ inTrain,]
    testing = concrete[-inTrain,]
    
    set.seed(325)
    library(e1071)
    
    svm_model <- svm(CompressiveStrength~., data=training)
    #summary(svm_model)
    pred <- predict(svm_model, testing)
    print(paste0("RMSE = ", sqrt(mean((pred - testing$CompressiveStrength)^2))))
}

Qn5()




