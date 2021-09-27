
Project <- function() {
    library(caret)
    library(gbm)
    set.seed(3433)
    library(AppliedPredictiveModeling)
    
    training_data_file = "./data/pml-training.csv"
    if (!file.exists(training_data_file)) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(fileURL,destfile=training_data_file)
    }
    raw_train <- read.csv(training_data_file, header=T, na.strings=c("","NA","#DIV/0!"))
    raw_train$classe <- as.factor(raw_train$classe)
    
    test_data_file <- "./data/pml-testing.csv"
    if (!file.exists(test_data_file)) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(fileURL,destfile=training_data_file)
    }
    
    raw_test <- read.csv(test_data_file, header=T, na.strings=c("","NA","#DIV/0!"))
    
    #str(raw_train)
    #dim(raw_train)
    
    NAindex <- apply(raw_train,2,function(x) {sum(is.na(x))}) 
    raw_train <- raw_train[,which(NAindex == 0)]
    raw_test <- raw_test[,which(NAindex == 0)]
    
    num <- which(lapply(raw_train, class) %in% "numeric")
    preObj <-preProcess(raw_train[,num],method=c('knnImpute', 'center', 'scale'))
    clean_train <- predict(preObj, raw_train[,num])
    clean_train$classe <- raw_train$classe
    clean_test <- predict(preObj, raw_test[,num])
    
    nzv <- nearZeroVar(clean_train,saveMetrics=TRUE)
    clean_train <- clean_train[,nzv$nzv==FALSE]
    nzv <- nearZeroVar(clean_test,saveMetrics=TRUE)
    clean_test <- clean_test[,nzv$nzv==FALSE]
    
    str(clean_train)
    
    # correlate the predictors by removing the classe column and then looking at correlation
    # this will help to reduce the number of predictors and the noise
    # Find a new set of variable that are uncorrelated and explain as much of the variance as possible
    # M <- abs(cor(select(clean_train,-classe)))
    # diag(M) <- 0
    # which(M > 0.8,arr.ind=T)
    
    set.seed(62433)
    # Create cross validation set
    inTrain = createDataPartition(clean_train$classe, p = 3/4, list=FALSE)
    training = clean_train[inTrain,]
    testing = clean_train[-inTrain,]

    modelFit_rf <- train(classe~.,data=training,method="rf")
    pred_rf_tr <- predict(modelFit_rf,training)
    pred_rf <- predict(modelFit_rf,testing)
    print(paste0("Random Forrest ",confusionMatrix(testing$classe,pred_rf)$overall["Accuracy"]))
    
    print("TRAINING")
    cf_in_sample = confusionMatrix(training$classe,pred_rf_tr)
    print(cf_in_sample)
    
    print("TESTING")
    cf_out_sample = confusionMatrix(testing$classe,pred_rf)
    print(cf_out_sample)

}
Project()
