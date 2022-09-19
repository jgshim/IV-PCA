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
train2 <- smote(PONV~., train, perc.over = 2, perc.under=2)
table(train2$PONV)
train <- train2n
t Boosting ???? ????

#modelmodel on the training set
set.seed(42)

train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              savePredictions = TRUE)

# This is the G search for the best hyperparameters.
# The hyperpexpand.grid(nrounds = c(100,200),
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)


model <- caet::train(PONV ~ ., data = train,
                      method = "xgbTree",
                      trControl = train.control)

model
varImp(model)

# Best tuning parameter mtry
model$bestTune


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

library(epiR)
tabl
