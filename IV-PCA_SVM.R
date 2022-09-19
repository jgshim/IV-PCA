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


# 2. SVM Raial ???? ????

#model� ????

sbase model2)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 5,
                              classProbs = TRUE)

svmModel1 <- caret::train(PONV ~ ., data = train,
                          method = "svmPoly",
                          trControl = train.control,
                          metric = "Accuracy")

ggplot(svmModel1)
svmModel1
varImp(svmModel1)

# 2-2. ?׸???Ž??

Grid Searchpand.grid(sigma=c(0,0.01, 0.02, 0.025, 0.03, 0.04, 0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                    C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2,5))  

set.seed(42)
svmModel2 <- train(OST ~ caret::., datPONVata_train,
              method="svmR       method="svmRadial",
              trControl=train.control,
                   metric = "Ac       metric = "Accuracy",
              tuneGrid = Grid)
svmModel2

t
rellis.par.set(caretTheme())
plot(svmModel2) 
plot(svmModel2, metric = "Kappa")
plot(svmModel2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(svmModel2) 

# 2-3. ???? ??ġ ??Random search-based model tuningrol <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 10,
                              classProbs = TRUE,
                              search = "random")
set.seed(42)
svmModel3 <- train(OST ~ caret::., datPONV data_train,       
                          method = "svmRadial",
                          metric = "Accuracy",
                          tuneLength = 30,
                          train.control)
svmModel3

plot(svmModel3) 
plot(svmModel3, metric = "Kappa")
plot(svmModel3, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(svmModel3) 

# 3. ???? ????

#Model performance
??????? Training model
pedict(svmModel1, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2) ?׽?Ʈ ?????? ??Test modelct(svmModel1, test)
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes")
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$PONV)

# 3-3) ROC

require(Epi)
require(pROC)

predictedProbs <- predict(svmModel1, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = PONV ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(PONV ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(ROCR)
libra