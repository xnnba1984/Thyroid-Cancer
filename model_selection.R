library(randomForest)
library(caret)
library(doParallel)
library(PRROC)
setwd("C:/Users/mxi1/Dropbox/medical/2022")

##########################################################################
# descriptive statistics
##########################################################################
data <- readRDS('result/thyroid_clean.rds')
table(data$id)
length(unique(data$id))

# remove id
data <- data[,-1]
str(data)
summary(data)
median(data$age);IQR(data$age)

table(data$gender)/dim(data)[1]
median(data$FT3);IQR(data$FT3)
median(data$FT4);IQR(data$FT4)
median(data$TSH);IQR(data$TSH)
median(data$TPO);IQR(data$TPO)
median(data$TGAb);IQR(data$TGAb)

mean(data$FT3);sd(data$FT3)
mean(data$FT4);sd(data$FT4)
mean(data$TSH);sd(data$TSH)
mean(data$TPO);sd(data$TPO)
mean(data$TGAb);sd(data$TGAb)

table(data$site)/dim(data)[1]
table(data$echo_pattern)/dim(data)[1]
table(data$multifocality)/dim(data)[1]

mean(data$size);sd(data$size)
median(data$size);IQR(data$size)

table(data$shape)/dim(data)[1]
table(data$margin)/dim(data)[1]
table(data$calcification)/dim(data)[1]
table(data$echo_strength)/dim(data)[1]
table(data$blood_flow)/dim(data)[1]
table(data$composition)/dim(data)[1]
table(data$multilateral)/dim(data)[1]
table(data$mal)/dim(data)[1]

##########################################################################
# logistic regression
##########################################################################
system.time({
acc <- c()
auroc <- c()
sen <- c()
spec <- c()
prec <- c()

# repeat 10-fold cv by 10 times
for(seed in 1:10){
  print(seed)
  set.seed(seed)
  data <- readRDS('result/thyroid_clean.rds')
  index <- sample(unique(data$id))
  # 10-fold cross-validation by patients
  index.list <- split(index, cut(seq_along(index), 10, labels = FALSE))
  result <- data.frame()
  
  for(index.test in index.list){
    #index.test <- index.list[[1]]
    index.train <- setdiff(index, index.test)
    test <- data[data$id%in%index.test,]
    train <- data[data$id%in%index.train,]
    model <- glm(mal~., family = 'binomial', data = train[,-1])
    pred.prob <- predict(model, test[,-1], type = 'response')
    result <- rbind(result, cbind(row.names(pred.prob), pred.prob))
  }
  
  data[row.names(result), 'pred.prob'] <- result$pred.prob
  data$pred.mal <- as.factor(ifelse(data$pred.prob >= 0.5, 1, 0))
  cm <- confusionMatrix(data$pred.mal, data$mal, mode = 'everything', positive = '1')
  roc <- roc.curve(scores.class0 = data[data$mal=='1','pred.prob'], 
                   scores.class1 = data[data$mal=='0','pred.prob'], curve=F); roc$auc
  acc[seed] <- cm[["overall"]][["Accuracy"]]
  auroc[seed] <- roc$auc
  sen[seed] <- cm[["byClass"]][["Sensitivity"]]
  spec[seed] <- cm[["byClass"]][["Specificity"]]
  prec[seed] <- cm[["byClass"]][["Precision"]]
}
})
mean(acc); mean(auroc); mean(sen); mean(spec); mean(prec)

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

##########################################################################
# lda, qda, naive bayes
##########################################################################
library(MASS)
library(e1071)
system.time({
  acc <- c()
  auroc <- c()
  sen <- c()
  spec <- c()
  prec <- c()
  for(seed in 1:10){
    print(seed)
    set.seed(seed)
    data <- readRDS('result/thyroid_clean.rds')
    index <- sample(unique(data$id))
    index.list <- split(index, cut(seq_along(index), 10, labels = FALSE))
    result <- data.frame()
    
    for(index.test in index.list){
      #index.test <- index.list[[2]]
      index.train <- setdiff(index, index.test)
      test <- data[data$id%in%index.test,]
      train <- data[data$id%in%index.train,]
      
      # lda
      model <- lda(mal~., data=train[,-1])
      pred <- predict(model, test[,-1])
      pred.prob <- pred[["posterior"]][,2]
      
      # qda
      #model <- qda(mal~., data=train)
      #pred <- predict(model, test)
      #pred.prob <- pred[["posterior"]][,2]
      
      # naive bayes
      #model <- naiveBayes(mal~., data=train)
      #pred <- predict(model, test, type = 'raw')
      #pred.prob <- pred[,2]
      
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
  }
})
mean(acc); mean(auroc); mean(sen); mean(spec); mean(prec)

##########################################################################
# gbm
##########################################################################
library(gbm)
system.time({
  acc <- c()
  auroc <- c()
  sen <- c()
  spec <- c()
  prec <- c()
  for(seed in 1:10){
    print(seed)
    set.seed(seed)
    data <- readRDS('result/thyroid_clean.rds')
    #
    data$mal <- as.character(data$mal)
    #
    index <- sample(unique(data$id))
    index.list <- split(index, cut(seq_along(index), 10, labels = FALSE))
    result <- data.frame()
    
    for(index.test in index.list){
      #index.test <- index.list[[1]]
      index.train <- setdiff(index, index.test)
      test <- data[data$id%in%index.test,]
      train <- data[data$id%in%index.train,]
      
      model <- gbm(mal~., data=train[,-1], verbose = F)
      pred.prob <- predict(object = model, newdata = test[,-1], type = 'response')
      result <- rbind(result, cbind(row.names(test), pred.prob))
    }
    data[result$V1, 'pred.prob'] <- result$pred.prob
    #
    data$mal <- as.factor(data$mal)
    #
    data$pred.mal <- as.factor(ifelse(data$pred.prob >= 0.5, 1, 0))
    cm <- confusionMatrix(data$pred.mal, data$mal, mode = 'everything', positive = '1'); cm[["overall"]][["Accuracy"]]
    roc <- roc.curve(scores.class0 = data[data$mal=='1','pred.prob'], 
                     scores.class1 = data[data$mal=='0','pred.prob'], curve=F); roc$auc
    acc[seed] <- cm[["overall"]][["Accuracy"]]
    auroc[seed] <- roc$auc
    sen[seed] <- cm[["byClass"]][["Sensitivity"]]
    spec[seed] <- cm[["byClass"]][["Specificity"]]
    prec[seed] <- cm[["byClass"]][["Precision"]]
  }
})
mean(acc); mean(auroc); mean(sen); mean(spec); mean(prec)

##########################################################################
# svm
##########################################################################
library(e1071)
system.time({
  acc <- c()
  auroc <- c()
  sen <- c()
  spec <- c()
  prec <- c()
  for(seed in 1:10){
    print(seed)
    set.seed(seed)
    data <- readRDS('result/thyroid_clean.rds')
    index <- sample(unique(data$id))
    index.list <- split(index, cut(seq_along(index), 10, labels = FALSE))
    result <- data.frame()
    
    for(index.test in index.list){
      index.train <- setdiff(index, index.test)
      test <- data[data$id%in%index.test,]
      train <- data[data$id%in%index.train,]
      
      #model <- svm(mal~., kernel='sigmoid', data=train, probability=T)
      model <- svm(mal~., kernel='radial', data=train[,-1], probability=T)
      #model <- svm(mal~., kernel='linear', data=train[,-1], probability=T)
      pred.prob <- attr(predict(model, test[,-1], probability=T),'probabilities')[,1]
      result <- rbind(result, cbind(row.names(test), pred.prob))
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
  }
})
mean(acc); mean(auroc); mean(sen); mean(spec); mean(prec)






