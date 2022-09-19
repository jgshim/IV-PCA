#---- Preparation for IV-PCA data analysis ----

# Prepare data

# Library load
library(tidyverse)
library(caret)
library(performanceEstimation)

########### Binary classification ###########

# 1. Import processed data

data <- read.csv("data.csv", header=TRUE)
data <- data[ , c(2:37)]
head(data)

table(data$type_an)

# patients under general anesthesia (G/A)
data <- data %>% filter(type_an == 1)

data$PONV <- factor(data$PONV)

# prepare training scheme

data <- select(data,motion_sickness,main_fentanyl,age,bmi,asa,ponv,premedi,type_op.7,duration_an,sex,lapa,htn,pre_op,PONV)

set.seed(42)
training.samples <- createDataPartition(data$PONV, p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

table(train$PONV)
table(test$PONV)

# SMOTE
train2 <- smote(PONV~., train, perc.over = 2, perc.under=2n2$PONV)
train <- train2

# 2. k-nearest neighbors (KNN) ???? ????

#modelaresl on the training set
set.seed(42)


train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 3,
                              savePredictions = TRUE)

model <- caret::train(PONV ~ ., data = train,
                      method = "knn",
                      trControl = train.control,
                      tuneLength = 20)

model

# Plot model accuracy vs different values of k
plot(model)


# Print the best tuning parameter k that maximizes model accuracy
model$bestTu
ne

# 3. ???? ????

#Model performance

# 3-1) Training modelpredict(model, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2. ?׽?Ʈ ?????? ??Test modelct(model, test)
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes")
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$PONV)

# 3-3. ROC

require(Epi)
require(pROC)

predictedProbs <- predict(model, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = PONV ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(PONV ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(ROCR)
libr