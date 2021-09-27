

library(AppliedPredictiveModeling)
library(dplyr)
library(Hmisc)
library(gridExtra)

# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
# 3. In the final model what would be the final model prediction for cases with the following variable values:
Qn1 <- function() {
    RNGversion("3.0.0")
    library(AppliedPredictiveModeling)
    data(segmentationOriginal)
    library(caret)
    library(rattle)
    set.seed(125)
    inTrain = createDataPartition(segmentationOriginal$Case,p=0.7,list=FALSE)
    training = segmentationOriginal[ inTrain,]
    testing = segmentationOriginal[-inTrain,]
    #dim(training);dim(testing)
    summary(training)
    
    modFit <- train(Class~.,method="rpart",data=training)
    print(modFit$finalModel)
    fancyRpartPlot(modFit$finalModel)
}

Qn1()

# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample 
# (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out 
# cross validation? 

# ANS -The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size. 

# These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
# Fit a classification tree where Area is the outcome variable. Then predict the value of area for 
# the following data frame using the tree command with all defaults
# What is the resulting prediction? Is the resulting prediction strange? Why or why not?

# ANS - The predicted value is 2.783. It is strange because Area should be a qualitative variable - 
# but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata.
Qn3 <- function() {
    library(pgmm)
    data(olive)
    olive = olive[,-1]
    print(olive)
    modFit <- train(Area~.,method="rpart",data=olive)
    newdata = as.data.frame(t(colMeans(olive)))
    predict(modFit,newdata)
}

Qn3()

# Then set the seed to 13234 and fit a logistic regression model  (method="glm", 
# be sure to specify family="binomial") with Coronary Heart Disease (chd) as the 
# outcome and age at onset, current alcohol consumption, obesity levels, cumulative 
# tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors. 
# Calculate the misclassification rate for your model using this function and a 
# prediction on the "response" scale:

missClass <- function(values,prediction){
    sum(((prediction > 0.5)*1) != values)/length(values)
    }

Qn4 <- function() {
    library(ElemStatLearn)
    data(SAheart)
    set.seed(8484)
    train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
    trainSA = SAheart[train,]
    testSA = SAheart[-train,]
    set.seed(13234)
    #summary(trainSA)
    modelFit <- train(chd~age + alcohol + obesity + tobacco + typea + ldl, data=trainSA,method="glm",family="binomial")
    
    print(missClass(testSA$chd,predict(modelFit,testSA)))
    
    print(missClass(trainSA$chd,predict(modelFit,trainSA)))
    
}

Qn4()

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. 
# Fit a random forest predictor relating the factor variable y to the remaining variables. Read about variable 
# importance in random forests here:  http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
# The caret package uses by default the Gini importance. 

# Calculate the variable importance using the varImp function in the caret package. What is the order of variable importance?
#[NOTE: Use randomForest() specifically, not caret, as there's been some issues reported with that approach. 11/6/2016]

Qn5 <- function() {
    library(ElemStatLearn)
    data(vowel.train)
    data(vowel.test)
    #summary(vowel.train)
    
    vowel.test$y <- factor(vowel.test$y)
    vowel.train$y <- factor(vowel.train$y)
    set.seed(33833)
    #str(vowel.train)
    modelFit <- train(y~.,data=vowel.train,method="rf",prox=TRUE)
    varImp(modelFit)
}

Qn5()





