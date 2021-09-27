

library(AppliedPredictiveModeling)
library(dplyr)
library(Hmisc)
library(gridExtra)

# Which of the following commands will create non-overlapping training and 
# test sets with about 50% of the observations assigned to each?
Qn1 <- function() {
    data(AlzheimerDisease)
    
    # this is the correct answer
    adData = data.frame(diagnosis,predictors)
    trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
    training = adData[trainIndex,]
    testing = adData[-trainIndex,]
    dim(training)
}

Qn1()

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function in 
# the Hmisc package useful for turning continuous covariates into factors).
# What do you notice in these plots?
# ANS - There is a non-random pattern in the plot of the outcome versus index that does 
# not appear to be perfectly explained by any predictor suggesting a variable may be missing.
Qn2 <- function() { 
    data(concrete)
    library(caret)
    set.seed(1000)
    inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
    training = mixtures[ inTrain,]
    testing = mixtures[-inTrain,]
    
    breaks = 5
    training <- mutate(training, index=1:nrow(training))
    p1 <- qplot(index,CompressiveStrength, data=training, color=cut2(Cement, g=breaks))
    p2 <- qplot(index,CompressiveStrength, data=training, colour=cut2(BlastFurnaceSlag, g=breaks))
    p3 <- qplot(index,CompressiveStrength, data=training, colour=cut2(FlyAsh, g=breaks))
    p4 <- qplot(index,CompressiveStrength, data=training, colour=cut2(Water, g=breaks))
    p5 <- qplot(index,CompressiveStrength, data=training, colour=cut2(Superplasticizer, g=breaks))
    p6 <- qplot(index,CompressiveStrength, data=training, colour=cut2(CoarseAggregate, g=breaks))
    p7 <- qplot(index,CompressiveStrength, data=training, colour=cut2(FineAggregate, g=breaks))
    p8 <- qplot(index,CompressiveStrength, data=training, colour=cut2(Age, g=breaks))
    grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8 ,nrow = 4)
    #str(training)
}

Qn2()

# Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more symmetric. 
# Why would that be a poor choice for this variable?

# ANS - The log transform does not reduce the skewness of the non-zero values of SuperPlasticizer
Qn3 <- function() {
    library(AppliedPredictiveModeling)
    library(gridExtra)
    data(concrete)
    library(caret)
    set.seed(1000)
    inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
    training = mixtures[ inTrain,]
    testing = mixtures[-inTrain,]
    #summary(training)
    p1 <- hist(training$Superplasticizer)
    #p2 <- hist(log(training$Superplasticizer+1))
    
    #grid.arrange(p1, p2, nrow = 1)
}

Qn3()

# Find all the predictor variables in the training set that begin with IL. Perform principal 
# components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 90% of the variance. 
# How many are there?

# there are 7 principal components
Qn4 <- function() {
    library(caret)
    library(AppliedPredictiveModeling)
    library(dplyr)
    set.seed(3433)
    data(AlzheimerDisease)
    adData = data.frame(diagnosis,predictors)
    inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
    training = adData[ inTrain,]
    testing = adData[-inTrain,]
    
    # new code
    IL_col_idx <- grep("^[Ii][Ll].*", names(training))
    new_training <- training[, c(names(training)[IL_col_idx], "diagnosis")]
    #print(names(new_training))

    preProc <- preProcess(new_training, method = "pca", thresh = 0.9)
    preProc$rotation
}

Qn4()

# Create a training data set consisting of only the predictors with variable names 
# beginning with IL and the diagnosis. Build two predictive models, one using the
# predictors as they are and one using PCA with principal components explaining 80% 
# of the variance in the predictors. Use method="glm" in the train function. 

# What is the accuracy of each method in the test set? Which is more accurate?
Qn5 <- function() {
    set.seed(3433)
    library(AppliedPredictiveModeling)
    data(AlzheimerDisease)
    adData = data.frame(diagnosis, predictors)
    inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
    training = adData[inTrain, ]
    testing = adData[-inTrain, ]
    
    set.seed(3433)
    suppressMessages(library(dplyr))
    IdxCol_IL <- grep("^IL", names(testing))
    names_IL <- names(testing[,IdxCol_IL])
    newcols <- c(names_IL,"diagnosis")
    new_testing <- testing [,newcols]
    new_training <- training[,newcols]
    
    # Model 1 : predictors as they are, without PCA
    model_without_PCA <- train(diagnosis~., data=new_training,   preProcess=c("center","scale"),method="glm")
    model_result_without_PCA <- confusionMatrix(new_testing$diagnosis,predict(model_without_PCA,subset(new_testing, select = -c(diagnosis))))
    print(model_result_without_PCA)
    
    preProc_pca <-  preProcess(subset(new_training, select = -c(diagnosis)), method="pca", thresh=0.8)
    
    trainPC <- predict(preProc_pca,subset(new_training, select = -c(diagnosis)))
    testPC <- predict(preProc_pca,subset(new_testing, select = -c(diagnosis)))
    # Syntax to use to avoid "undefined columns selected" error message (by following the formula  defined in the slides.)
    model_with_PCA<- train(x = trainPC, y = new_training$diagnosis,method="glm") 
    model_result_with_PCA <- confusionMatrix(new_testing$diagnosis,predict(model_with_PCA, newdata=testPC))
    print(model_result_with_PCA)

}

Qn5()





