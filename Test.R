

Project <- function() {
    library(caret)
    library(gbm)
    set.seed(3433)
    library(AppliedPredictiveModeling)
    
    training_data_file = "./data/pml-training.csv"
    if (!file.exists(training_data_file)) {
        # downloading the data file
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(fileURL,destfile=training_data_file)
    }
    
    training <- read.csv(training_data_file)
    #str(training)
    
    test_data_file = "./data/pml-testing.csv"
    if (!file.exists(test_data_file)) {
        # downloading the data file
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(fileURL,destfile=training_data_file)
    }

    testing <- read.csv(test_data_file)
    # print(testing[1,])
    
    training$classe <- as.factor(training$classe)
    #testing$classe <- as.factor(testing$classe)
    
    # Cleaning variables
    NAindex <- apply(training,2,function(x) {sum(is.na(x))})
    training <- training[,which(NAindex == 0)]
    NAindex <- apply(testing,2,function(x) {sum(is.na(x))})
    testing <- testing[,which(NAindex == 0)]
    
    # # Preprocessing variables
    v <- which(lapply(training, class) %in% "numeric")
    preObj <-preProcess(training[,v],method=c('knnImpute', 'center', 'scale'))
    trainLess1 <- predict(preObj, training[,v])
    trainLess1$classe <- training$classe

    testLess1 <-predict(preObj,testing[,v])
    # 
    # # Removing the non zero variables
    nzv <- nearZeroVar(trainLess1,saveMetrics=TRUE)
    trainLess1 <- trainLess1[,nzv$nzv==FALSE]
    str(trainLess1)
    # 
    # # nzv <- nearZeroVar(testLess1,saveMetrics=TRUE)
    # # testLess1 <- testLess1[,nzv$nzv==FALSE]
    # 
    # set.seed(62433)
    # 
    # # Create cross validation set
    # inTrain = createDataPartition(trainLess1$classe, p = 3/4, list=FALSE)
    # tr = trainLess1[inTrain,]
    # te = trainLess1[-inTrain,]
    # 
    # str(tr)
    
    # modelFit_rf <- train(classe~.,data=tr,method="rf")
    # pred_rf <- predict(modelFit_rf,te)
    # print(paste0("Random Forrest ",confusionMatrix(te$classe,pred_rf)$overall["Accuracy"]))

    # modelFit_boost <- train(classe~.,data=tr,method="gbm",verbose=FALSE)
    # pred_boost <- predict(modelFit_boost,te)
    # print(paste0("Boost ",confusionMatrix(te$classe,pred_boost)$overall["Accuracy"]))
    # 
    # modelFit_lda <- train(classe~.,data=tr,method="lda")
    # pred_lda <- predict(modelFit_lda,te)
    # print(paste0("LDA ",confusionMatrix(te$classe,pred_lda)$overall["Accuracy"]))
    # 
    # predDF <- data.frame(pred_rf,pred_boost,pred_lda,classe=te$classe)
    # CombModFit <- train(classe~.,method="rf",data=predDF)
    # pred_comb <- predict(CombModFit,predDF)
    # print(paste0("STACKED ",confusionMatrix(te$classe,pred_comb)$overall["Accuracy"]))
    
}
Project()
