beltC <- c(grep("belt", predictors), 53)
dumbC <- c(grep("dumbbell", predictors), 53)
armC <- c(grep("[^(fore)]arm", predictors), 53)
foreC <- c(grep("forearm", predictors), 53)

library(reshape2)

groupViolin <- function(groupC){
    groupM <- melt(pml_training[, groupC], id.vars = "classe")
    ggplot(groupM, aes(x=classe, y=value)) +
        geom_violin(aes(color=classe, fill=classe), alpha=1/2) +
        facet_wrap(~ variable, scale="free_y") +
        scale_color_brewer(palette="Set1") +
        scale_fill_brewer(palette="Set1") +
        labs(x="", y="") +
        theme(legend.position="none")
}

groupViolin(beltC)
groupViolin(dumbC)
groupViolin(armC)
groupViolin(foreC)

library(caret)

# Train control across models
trCtrl <- trainControl(method="repeatedcv", number=5, repeats=1,
                       verboseIter=FALSE, classProbs=TRUE, allowParallel=TRUE)

## set seed
set.seed(300369)

inTrain <- createDataPartition(y=pml_training$classe, p=0.6, list=FALSE)
training <- pml_training[inTrain, ]
testing <- pml_training[-inTrain, ]
dim(training)
dim(testing)

# Preprocessing

library("data.table")

preProc <- preProcess(training[1:52])
predppTrain <- predict(preProc, training[1:52])
trainingCS <- data.table(data.frame(classe = training$classe, predppTrain))

predppTest <- predict(preProc, testing[1:52])
testingCS <- data.table(data.frame(classe = testing$classe, predppTest))

# Decision tree

treeFit <- train(classe ~., data=trainingCS, method="rpart", trControl=trCtrl)
treePred <- predict(treeFit, newdata=testingCS)
treeCM <- confusionMatrix(treePred, testingCS$classe)

library("rattle")
library("rpart.plot")
fancyRpartPlot(treeFit$finalModel)

# Linear Discriminant Analysis

ldaFit <- train(classe ~., data=trainingCS, method="lda", trControl=trCtrl)
ldaPred <- predict(ldaFit, newdata=testingCS)
ldaCM <- confusionMatrix(ldaPred, testingCS$classe)

# Random Forest

rfFit <- train(classe ~., data=trainingCS, method="rf", trControl=trCtrl)
rfPred <- predict(rfFit, newdata=testingCS)
rfCM <- confusionMatrix(rfPred, testingCS$classe)

pml_testingCS <- predict(preProc, pml_testing)
rfPredTesting <- predict(rfFit, newdata=pml_testingCS)
rfPredTesting
