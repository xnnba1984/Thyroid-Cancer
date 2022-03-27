library(tidyverse)
library(readxl)
setwd("C:/Users/mxi1/Dropbox/medical/2021")

data <- read_excel("thyroid.xlsx");str(data)

# use max length as final length
length <- cbind(data$length1, data$length2, data$length3, data$length4, data$length5)
length.max <- apply(length, 1, max, na.rm=T)

# use max width as final width
width <- cbind(data$width1, data$width2, data$width3, data$width4, data$width5)
width.max <- apply(width, 1, max, na.rm=T)

# use the max of length and width as size
size <- cbind(length.max, width.max)
size.max <- apply(size, 1, max)


# indicate irregular if one node is irregular
shape <- cbind(data$shape1, data$shape2, data$shape3, data$shape4, data$shape5)
shape.max <- apply(shape, 1, max, na.rm=T)

# indicate unclear boundary if one node is boundary
margin <- cbind(data$margin1, data$margin2, data$margin3, data$margin4, data$margin5)
margin.max <- apply(margin, 1, max, na.rm=T)

# indicate calcification if one node is calcificated
calcification <- cbind(data$calcification1, data$calcification2, data$calcification3, data$calcification4, data$calcification5)
calcification.max <- apply(calcification, 1, max, na.rm=T)

# use the max of echo as echo_strength
echo_strength <- cbind(data$echo_strength1, data$echo_strength2, data$echo_strength3, data$echo_strength4, data$echo_strength5)
echo_strength.max <- apply(echo_strength, 1, max, na.rm=T)

# indicate enriched blood flow if one node is enriched 
blood_flow <- cbind(data$blood_flow1, data$blood_flow2, data$blood_flow3, data$blood_flow4, data$blood_flow5)
blood_flow.max <- apply(blood_flow, 1, max, na.rm=T)

# use the max of composition as composition
composition <- cbind(data$composition1, data$composition2, data$composition3, data$composition4, data$composition5)
composition.max <- apply(composition, 1, max, na.rm=T)

# combine to get whole dataset
data.clean <- cbind(data[,1:11], size.max, shape.max, margin.max, calcification.max, 
                    echo_strength.max, blood_flow.max, composition.max, data$mal)
colnames(data.clean)[12:19] <- c('size', 'shape', 'margin', 'calcification', 'echo_strength', 'blood_flow', 'composition', 'mal')

data.clean$gender <- as.factor(data.clean$gender)
data.clean$site <- as.factor(data.clean$site)
data.clean$echo_pattern <- as.factor(data.clean$echo_pattern)
data.clean$multifocality <- as.factor(data.clean$multifocality)
data.clean$shape <- as.factor(data.clean$shape)
data.clean$margin <- as.factor(data.clean$margin)
data.clean$calcification <- as.factor(data.clean$calcification)
data.clean$echo_strength <- as.factor(data.clean$echo_strength)
data.clean$blood_flow <- as.factor(data.clean$blood_flow)
data.clean$composition <- as.factor(data.clean$composition)

# find multilateral nodes
multilateral <- duplicated(data.clean$id) | duplicated(data.clean$id, fromLast = T)
multilateral <- ifelse(multilateral==T, 1, 0)
data.clean$multilateral <- as.factor(multilateral)
data.clean$mal <- as.factor(data.clean$mal); str(data.clean)
saveRDS(data.clean, 'result/thyroid_clean.rds')


