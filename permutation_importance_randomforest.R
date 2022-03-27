library(randomForest)
library(caret)
library(doParallel)
library(PRROC)
setwd("C:/Users/nan/Desktop/medical/2021")

##########################################################################
# random forest
##########################################################################
set.seed(2021)
acc.importance <- c()
auroc.importance <- c()

system.time({
for(j in c(1:17, 19)){
  data <- readRDS('thyroid_clean.rds')
  data <- data[,-1]
  cat(colnames(data)[j], '\n')
  index.list <- createFolds(1:dim(data)[1], k = 10, list = T, returnTrain = F); index.list[[1]]
  result <- data.frame()
  
  for(index.test in index.list){
    #index.test <- index.list[[1]]
    index.train <- setdiff(1:dim(data)[1], index.test)
    test <- data[index.test,]
    train <- data[index.train,]
    
    model <- randomForest(mal~., data=train)
    
    result.feature <- c()
    # repeat random shuffling
    for(i in 1:100){
      #cat("repeat============", i, '\n')
      test[,j] <- test[sample(dim(test)[1], replace = F), j]
      pred.prob <- predict(model, test, type = 'prob')[,2]
      result.feature <- cbind(result.feature, pred.prob)
    }
    result <- rbind(result, result.feature)
  }
  data <- merge(data, result, by = 0, all = T)
  data[,121:220] <- ifelse(data[,21:120] >= 0.5, 1, 0)
  data[,121:220] <- sapply(data[,121:220],factor)
  
  # acc
  acc.feature <- c()
  auroc.feature <- c()
  for(i in 121:220){
    cm <- confusionMatrix(factor(data[,i]), data$mal, mode = 'everything', positive = '1')
    acc.feature[i-120] <- cm[["overall"]][["Accuracy"]]
    roc <- roc.curve(scores.class0 = data[data$mal=='1',i-100], 
                     scores.class1 = data[data$mal=='0',i-100], curve=F);roc$auc
    auroc.feature[i-120] <- roc$auc
  }
  cat(mean(acc.feature), ' ', mean(auroc.feature), '\n')
  acc.importance[j] <- mean(acc.feature)
  auroc.importance[j] <- mean(auroc.feature)
}
})

result.importance <- data.frame(cbind(acc.importance, auroc.importance))
result.importance <- result.importance[complete.cases(result.importance),]
result.importance$feature <- colnames(data)[c(2:18, 20)]
saveRDS(result.importance, 'importance_randomforest.rds')

