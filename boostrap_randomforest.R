library(randomForest)
library(caret)
library(doParallel)
library(PRROC)
library(glmnet)
setwd("C:/Users/mxi1/Dropbox/medical/2022")

##########################################################################
# random forest
##########################################################################
system.time({
  acc <- c()
  auroc <- c()
  sen <- c()
  spec <- c()
  prec <- c()
  boost.count <- 1000
  set.seed(2021)
  for(i in 1:boost.count){
    data <- readRDS('result/thyroid_clean.rds')
    tryCatch({
    print(i)
    index <- sample(unique(data$id))
    index.list <- split(index, cut(seq_along(index), 10, labels = FALSE))
    result <- data.frame()
    
    for(index.test in index.list){
      #index.test <- index.list[[1]]
      index.train <- setdiff(index, index.test)
      test <- data[data$id%in%index.test,]
      train <- data[data$id%in%index.train,]
      
      # resample with replacement on training set
      boostrap.index <- sample(dim(train)[1], replace = T)
      train <- train[boostrap.index,]
      
      model <- randomForest(mal~., data=train[,-1])
      pred.prob <- predict(model, test[,-1], type = 'prob')[,2]
      result <- rbind(result, cbind(row.names(pred.prob), pred.prob))
    }
    
    data[row.names(result), 'pred.prob'] <- result$pred.prob
    data$pred.mal <- as.factor(ifelse(data$pred.prob >= 0.5, 1, 0))
    cm <- confusionMatrix(data$pred.mal, data$mal, mode = 'everything', positive = '1')
    roc <- roc.curve(scores.class0 = data[data$mal=='1','pred.prob'], 
                     scores.class1 = data[data$mal=='0','pred.prob'], curve=F); roc$auc
    acc[i] <- cm[["overall"]][["Accuracy"]]
    auroc[i] <- roc$auc
    sen[i] <- cm[["byClass"]][["Sensitivity"]]
    spec[i] <- cm[["byClass"]][["Specificity"]]
    prec[i] <- cm[["byClass"]][["Precision"]]
    },error=function(e){print('Error')})
  }
})
result.boostrap <- data.frame(cbind(acc, auroc, sen, spec, prec))
result.boostrap <- result.boostrap[complete.cases(result.boostrap),]

# acc
round(quantile(result.boostrap$acc, c(0.025, 0.975)),4)
cat('mean',round(mean(result.boostrap$acc),4), '\n')

# auroc
round(quantile(result.boostrap$auroc, c(0.025, 0.975)),4)
cat('mean',round(mean(result.boostrap$auroc),4), '\n')

# sensitivity
round(quantile(result.boostrap$sen, c(0.025, 0.975)),4)
cat('mean',round(mean(result.boostrap$sen),4), '\n')

# specificity
round(quantile(result.boostrap$spec, c(0.025, 0.975)),4)
cat('mean',round(mean(result.boostrap$spec),4), '\n')

# precision
round(quantile(result.boostrap$prec, c(0.025, 0.975)),4)
cat('mean',round(mean(result.boostrap$prec),4), '\n')

saveRDS(result.boostrap, "result/result_boostrap_randomforest.rds")



