---
title: 'Groupware Barbell Prediction'
author: "Christine Grayson"
output:
  html_document:
    df_print: paged
    self_contained: yes
    keep_md: yes
  pdf_document: default
---


## Executive Summary

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to predict the manner in which they performed the exercise. This manner is captured in the 'classe' variable of the data and has values as follows:

* (Class A) - exactly according to the specification
* (Class B) - throwing the elbows to the front 
* (Class C) - lifting the dumbbell only halfway 
* (Class D) - lowering the dumbbell only halfway 
* (Class E) - throwing the hips to the front 

Read more: http:/groupware.les.inf.puc-rio.br/har#ixzz4TjpsCvjA

## Loading Data






## Data Cleaning and Preprocessing
Remove columns that are totally NAs and those that are non numeric and then impute NA values, center and scale. The 'classe' value will be removed durint this process so we have to put it back again. Everything we do to the training we have to do to the test set that we will use to run the predictions on and answer the assignment.


```r
NAindex <- apply(raw_train,2,function(x) {sum(is.na(x))}) 
raw_train <- raw_train[,which(NAindex == 0)]
raw_test <- raw_test[,which(NAindex == 0)]

num <- which(lapply(raw_train, class) %in% "numeric")
preObj <-preProcess(raw_train[,num],method=c('knnImpute', 'center', 'scale'))
clean_train <- predict(preObj, raw_train[,num])
clean_train$classe <- raw_train$classe

clean_test <- predict(preObj, raw_test[,num])
```

## Covariates
Remove the near zero covariates, this removes the variables that have very little predictability and are probably not going to be very good predictors.


```r
nzv <- nearZeroVar(clean_train,saveMetrics=TRUE)
clean_train <- clean_train[,nzv$nzv==FALSE]
nzv <- nearZeroVar(clean_test,saveMetrics=TRUE)
clean_test <- clean_test[,nzv$nzv==FALSE]

str(clean_train)
```

```
## 'data.frame':	19622 obs. of  28 variables:
##  $ roll_belt        : num  -1 -1 -1 -1 -1 ...
##  $ pitch_belt       : num  0.347 0.347 0.347 0.347 0.347 ...
##  $ yaw_belt         : num  -0.874 -0.874 -0.874 -0.874 -0.874 ...
##  $ gyros_belt_x     : num  0.027 0.123 0.027 0.123 0.123 ...
##  $ gyros_belt_y     : num  -0.506 -0.506 -0.506 -0.506 -0.25 ...
##  $ gyros_belt_z     : num  0.458 0.458 0.458 0.417 0.458 ...
##  $ roll_arm         : num  -2 -2 -2 -2 -2 ...
##  $ pitch_arm        : num  0.884 0.884 0.884 0.871 0.871 ...
##  $ yaw_arm          : num  -2.25 -2.25 -2.25 -2.25 -2.25 ...
##  $ gyros_arm_x      : num  -0.0215 -0.0114 -0.0114 -0.0114 -0.0215 ...
##  $ gyros_arm_y      : num  0.302 0.278 0.278 0.267 0.267 ...
##  $ gyros_arm_z      : num  -0.523 -0.523 -0.523 -0.451 -0.487 ...
##  $ roll_dumbbell    : num  -0.154 -0.153 -0.157 -0.149 -0.15 ...
##  $ pitch_dumbbell   : num  -1.61 -1.62 -1.61 -1.61 -1.61 ...
##  $ yaw_dumbbell     : num  -1.05 -1.05 -1.05 -1.05 -1.05 ...
##  $ gyros_dumbbell_x : num  -0.107 -0.107 -0.107 -0.107 -0.107 ...
##  $ gyros_dumbbell_y : num  -0.108 -0.108 -0.108 -0.108 -0.108 ...
##  $ gyros_dumbbell_z : num  0.0564 0.0564 0.0564 0.0477 0.0564 ...
##  $ magnet_dumbbell_z: num  -0.793 -0.786 -0.779 -0.758 -0.815 ...
##  $ roll_forearm     : num  -0.0502 -0.0512 -0.0512 -0.053 -0.0539 ...
##  $ pitch_forearm    : num  -2.65 -2.65 -2.65 -2.65 -2.65 ...
##  $ yaw_forearm      : num  -1.67 -1.67 -1.66 -1.66 -1.66 ...
##  $ gyros_forearm_x  : num  -0.197 -0.213 -0.197 -0.213 -0.213 ...
##  $ gyros_forearm_y  : num  -0.0242 -0.0242 -0.0307 -0.0307 -0.0242 ...
##  $ gyros_forearm_z  : num  -0.0976 -0.0976 -0.0862 -0.0862 -0.0976 ...
##  $ magnet_forearm_y : num  0.538 0.551 0.546 0.546 0.54 ...
##  $ magnet_forearm_z : num  0.223 0.215 0.204 0.204 0.215 ...
##  $ classe           : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

## Selecting a Model on the Basis of Accuracy using Cross Validation
The first thing to do is to split the training set into a train portion and a test portion which will form the basis for cross validation. Then train various models against the train set and test the accuracy of each against the test set. There is even a stacked model which combines the models (taken from the example on the course). The conclusion is that the random forrest and stacked models have the greatest accuracy so use the random forrest model as the stacked has no benefit. 


```r
set.seed(62433)
# Create cross validation set
inTrain = createDataPartition(clean_train$classe, p = 3/4, list=FALSE)
train = clean_train[inTrain,]
test = clean_train[-inTrain,]

modelFit_rf <- train(classe~.,data=train,method="rf")
pred_rf <- predict(modelFit_rf,test)
print(paste0("Random Forrest Accuracy ",confusionMatrix(test$classe,pred_rf)$overall["Accuracy"]))
```

```
## [1] "Random Forrest Accuracy 0.996125611745514"
```

```r
modelFit_boost <- train(classe~.,data=train,method="gbm",verbose=FALSE)
pred_boost <- predict(modelFit_boost,test)
print(paste0("Boost Accuracy ",confusionMatrix(test$classe,pred_boost)$overall["Accuracy"]))
```

```
## [1] "Boost Accuracy 0.953303425774878"
```

```r
modelFit_lda <- train(classe~.,data=train,method="lda")
pred_lda <- predict(modelFit_lda,test)
print(paste0("LDA Accuracy ",confusionMatrix(test$classe,pred_lda)$overall["Accuracy"]))
```

```
## [1] "LDA Accuracy 0.507340946166395"
```

```r
predDF <- data.frame(pred_rf,pred_boost,pred_lda,classe=test$classe)
CombModFit <- train(classe~.,method="rf",data=predDF)
pred_comb <- predict(CombModFit,predDF)
print(paste0("STACKED Accuracy ",confusionMatrix(test$classe,pred_comb)$overall["Accuracy"]))
```

```
## [1] "STACKED Accuracy 0.996125611745514"
```

## In Sample Error, Versus Out of Sample Error
The in-sample error is the error rate you get on the same data set you use to build your prediction model (resubstitution error), the out-of-sample error is the error rate you get on new data (generalization error). Here we will just compare the two predictions for the random forrest model. As you can see below, the prediction is 100% accurate on the training data, but on the test data the out sample error misses 4 predictions. The  out sample error is expected to be higher the in-sample.

# Training confusion matrix showing in sample errors

```r
pred_rf_tr <- predict(modelFit_rf,train)
cf_in_sample = confusionMatrix(train$classe,pred_rf_tr)
print(cf_in_sample)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4185    0    0    0    0
##          B    0 2848    0    0    0
##          C    0    0 2567    0    0
##          D    0    0    0 2412    0
##          E    0    0    0    0 2706
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9997, 1)
##     No Information Rate : 0.2843     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1839
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

# Testing confusion matrix showing out sample errors

```r
cf_out_sample = confusionMatrix(test$classe,pred_rf)
print(cf_out_sample)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1394    1    0    0    0
##          B    1  945    3    0    0
##          C    0    5  847    3    0
##          D    0    0    4  799    1
##          E    0    0    1    0  900
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9961         
##                  95% CI : (0.994, 0.9977)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9951         
##                                          
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9993   0.9937   0.9906   0.9963   0.9989
## Specificity            0.9997   0.9990   0.9980   0.9988   0.9998
## Pos Pred Value         0.9993   0.9958   0.9906   0.9938   0.9989
## Neg Pred Value         0.9997   0.9985   0.9980   0.9993   0.9998
## Prevalence             0.2845   0.1939   0.1743   0.1635   0.1837
## Detection Rate         0.2843   0.1927   0.1727   0.1629   0.1835
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9995   0.9963   0.9943   0.9975   0.9993
```

## Running Predictions on the Test Set


```r
prediction <- predict(modelFit_rf, clean_test)
print(prediction)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```


