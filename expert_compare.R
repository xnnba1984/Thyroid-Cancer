library(readxl)
library(caret)
library(PRROC)
setwd("C:/Users/mxi1/Dropbox/medical/2022")

data <- readRDS('result/thyroid_clean.rds'); str(data)
data.expert <- data.frame(read_excel("thyroid_expert_result.xlsx"));str(data.expert)
expert.result <- factor(data.expert[,1])
cm <- confusionMatrix(expert.result, data$mal, mode = 'everything', positive = '1'); cm

##########################################################################
# random forest
##########################################################################
system.time({
  acc <- c()
  auroc <- c()
  sen <- c()
  spec <- c()
  prec <- c()
  f1 <- c()
  for(seed in 1:10){
    print(seed)
    set.seed(seed)
    data <- readRDS('result/thyroid_clean.rds')
    index <- sample(unique(data$id))
    # 10-fold cross-validation by patients
    index.list <- split(index, cut(seq_along(index), 10, labels = FALSE))
    result <- data.frame()
    
    for(index.test in index.list){
      #index.test <- index.list[[10]]
      index.train <- setdiff(index, index.test)
      test <- data[data$id%in%index.test,]
      train <- data[data$id%in%index.train,]
      model <- randomForest(mal~., data=train[,-1])
      pred.prob <- predict(model, test[,-1], type = 'prob')[,2]
      result <- rbind(result, cbind(row.names(pred.prob), pred.prob))
    }
    data[row.names(result), 'pred.prob'] <- result$pred.prob
    data$pred.mal <- as.factor(ifelse(data$pred.prob >= 0.5, 1, 0))
    cm <- confusionMatrix(data$pred.mal, data$mal, mode = 'everything', positive = '1'); cm[["overall"]][["Accuracy"]]
    roc <- roc.curve(scores.class0 = data[data$mal=='1','pred.prob'], 
                     scores.class1 = data[data$mal=='0','pred.prob'], curve=F); roc$auc
    acc[seed] <- cm[["overall"]][["Accuracy"]]
    auroc[seed] <- roc$auc
    sen[seed] <- cm[["byClass"]][["Sensitivity"]]
    spec[seed] <- cm[["byClass"]][["Specificity"]]
    prec[seed] <- cm[["byClass"]][["Precision"]]
    f1[seed] <- cm[["byClass"]][["F1"]]
  }
})
mean(acc); mean(auroc); mean(sen); mean(spec); mean(prec); mean(f1)


# both correct
expert.correct <- which(data$mal == expert.result)
model.correct <- which(data$mal == data$pred.mal)
correct.both <- intersect(expert.correct, model.correct); length(correct.both)

# both wrong
expert.wrong <- which(data$mal != expert.result)
model.wrong <- which(data$mal != data$pred.mal)
wrong.both <- intersect(expert.wrong, model.wrong); length(wrong.both)

# expert wrong & model correct 
expert.wrong.model.correct <- intersect(expert.wrong, model.correct); length(expert.wrong.model.correct)

# expert correct & model wrong 
expert.correct.model.wrong <- intersect(expert.correct, model.wrong); length(expert.correct.model.wrong)


