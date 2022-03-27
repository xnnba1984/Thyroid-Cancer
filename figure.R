library(ggplot2)
library(tidyr)
setwd("C:/Users/mxi1/Dropbox/medical/2022")

##########################################################################################################################
# uncertainty
##########################################################################################################################

gbm <- readRDS("result/result_boostrap_gbm.rds")
gbm$model <- "GBM"

lda <- readRDS("result/result_boostrap_lda.rds")
lda$model <- "LDA"

logistic <- readRDS("result/result_boostrap_logistic.rds")
logistic$model <- "Logistic"

svm.radial <- readRDS("result/result_boostrap_svm_radial.rds")
svm.radial$model <- "SVM (Radial)"

svm.linear <- readRDS("result/result_boostrap_svm_linear.rds")
svm.linear$model <- "SVM (Linear)"

randomforest <- readRDS("result/result_boostrap_randomforest.rds")
randomforest$model <- "Random Forest"

data <- rbind(gbm, logistic, lda, svm.radial, svm.linear, randomforest)
data.long <- gather(data, measurement, value, acc:prec, factor_key=TRUE)
data.long$measurement <- gsub('acc', 'Accuracy', data.long$measurement)
data.long$measurement <- gsub('auroc', 'AUROC', data.long$measurement)
data.long$measurement <- gsub('sen', 'Sensitivity', data.long$measurement)
data.long$measurement <- gsub('spec', 'Specificity', data.long$measurement)
data.long$measurement <- gsub('prec', 'Precision', data.long$measurement)
data.long$model <- gsub(" ", "\n", data.long$model)
str(data.long)

ggplot(data.long, aes(x = model, y = value, fill=model)) + geom_boxplot() +
  labs(x=NULL, y=NULL) + theme_bw() +
  facet_wrap(~ measurement, scales="free_y", ncol = 2) +
  theme(text=element_text(size = 25), axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), legend.position = "None") 

##########################################################################################################################
# importance based on auroc
##########################################################################################################################
importance.gbm <- readRDS('result/importance_gbm.rds')
importance.logistic <- readRDS('result/importance_logistic.rds')
importance.lda <- readRDS('result/importance_lda.rds')
importance.svm.radial <- readRDS('result/importance_svm_radial.rds')
importance.svm.linear <- readRDS('result/importance_svm_linear.rds')
importance.randomforest <- readRDS('result/importance_randomforest.rds')

#importance.gbm <- importance.gbm[order(importance.gbm$auroc.importance),]

importance.gbm$imp <- 0.8467 - importance.gbm$auroc.importance
importance.gbm$score <- importance.gbm$imp/max(importance.gbm$imp)

importance.logistic$imp <- 0.8409 - importance.logistic$auroc.importance
importance.logistic$score <- importance.logistic$imp/max(importance.logistic$imp)

importance.lda$imp <- 0.8383 - importance.lda$auroc.importance
importance.lda$score <- importance.lda$imp/max(importance.lda$imp)

importance.svm.radial$imp <- 0.8317 - importance.svm.radial$auroc.importance
importance.svm.radial$score <- importance.svm.radial$imp/max(importance.svm.radial$imp)

importance.svm.linear$imp <- 0.8308 - importance.svm.linear$auroc.importance
importance.svm.linear$score <- importance.svm.linear$imp/max(importance.svm.linear$imp)

importance.randomforest$imp <- 0.8301 - importance.randomforest$auroc.importance
importance.randomforest$score <- importance.randomforest$imp/max(importance.randomforest$imp)

result.importance <- cbind(importance.gbm$score, importance.logistic$score, importance.lda$score, 
                           importance.svm.radial$score, importance.svm.linear$score, importance.randomforest$score)
result.importance <- data.frame(cbind(importance.randomforest$feature, apply(result.importance, 1, mean)))
colnames(result.importance) <- c('feature', 'score')
result.importance$score <- as.numeric(result.importance$score)
#str(result.importance)
result.importance$score <- result.importance$score/max(result.importance$score)
result.importance <- result.importance[result.importance$score>0, ]
result.importance$feature <- gsub('calcification', 'Calcification', result.importance$feature)
result.importance$feature <- gsub('multilateral', 'Laterality', result.importance$feature)
result.importance$feature <- gsub('blood_flow', 'Blood Flow', result.importance$feature)
result.importance$feature <- gsub('site', 'Location', result.importance$feature)
result.importance$feature <- gsub('composition', 'Composition', result.importance$feature)
result.importance$feature <- gsub('size', 'Size', result.importance$feature)
result.importance$feature <- gsub('shape', 'Shape', result.importance$feature)
result.importance$feature <- gsub('margin', 'Margin', result.importance$feature)
result.importance$feature <- gsub('echo_strength', 'Echogenicity', result.importance$feature)
result.importance$feature <- gsub('multifocality', 'Multifocality', result.importance$feature)

ggplot(result.importance, aes(x=reorder(feature, score), y=score)) +
  geom_bar(stat="identity", fill='steelblue', width=0.7) + 
  coord_flip() + ylab('Normalized permutation predictor importance') +
  geom_text(aes(label=scales::percent(score, accuracy = 1L)), 
            hjust=ifelse(result.importance$score>0.04,1.1,-0.1), col=ifelse(result.importance$score>0.04, "white", "black"), size=6) +
  theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA), panel.background = element_blank(),
        text=element_text(size = 25))

#################################################################################################
# Variable
#################################################################################################
library("readxl")
library(tidytext)
data <- read_excel("result/variable.xlsx"); str(data)
data$variable = factor(data$variable, levels=c('Calcification','Laterality', 'Blood Flow', 
                                               'Location', 'Composition', 'Size (cm)'))

ggplot(data, aes(x = reorder(value, mal), y = mal)) + 
  geom_bar(stat='identity', width=0.3, fill='steelblue')+ 
  facet_wrap(~ variable, scale="free", ncol = 2) + theme_bw() +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=1.5) +
  xlab('\nVariable Value') + ylab('Malignancy Percentage\n') +
  theme(text=element_text(size = 22), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.spacing = unit(1, "lines"), axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15), axis.title=element_text(size=20)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels=scales::percent) +
  geom_text(aes(label=scales::percent(mal, accuracy = 1L)), vjust=-.5, col='black', size=5) + 
  geom_text(aes(0.4, 0.5, label='50%'), hjust=-.1, vjust=1.5, col='red')



