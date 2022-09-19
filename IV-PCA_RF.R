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


# 2. RandomForest ???? ????

#model» ????
trtraining modelol <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              savePredictions = TRUE,
                              classProbs =  TRUE)

RFModel1 <- caret::train(PONV ~ ., data=train,
                         method="rf",
                         trControl=train.control,
                         tuneLength = 10)

RFModel1
varImp(RFModel1)

# estimate variable importance
importance <- varImp(RFModel1)
# summarize importance
print(importance)
# plot importance
plot(importance)


# 2-2. ?×¸??Å½??

Grid Searchpand.grid(mtry = c(1:14)) 

set.seed(42)
train.control <- trainControl(method = "repeatedcv", 
                              number = 5, 
                              repeats = 10,
                              savePredictions = TRUE)

RFModel2 <- train(LBP ~ caret::., datPONVrain,
                  method="rf",       method="rf",
                         trControl=train.control,
                         metric = "Accuracy",
             tuneGrid = Grid)
RFModel2

trellis.par.set(caretTheme())
plot(RFModel2) 
plot(RFModel2, metric = "Kappa")
plot(RFModel2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(RFModel2) 

# 2-3. ???? ??Ä¡ ??Random search-based model tuningrol <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 10,
                              classProbs = TRUE,
                              search = "random")
set.seed(42)
RFModel3 <- train(LBP ~ caret::., datPONV train, 
          
             method = "rf",
           
                         metric = "Accuracy",
                         tuneLength = 30,
             trControl = train.control)
RFModel3

plot(RFModel3) 
plot(RFModel3, metric = "Kappa")
plot(RFModel3, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(RFModel3) 

# 2-4. ??Á¾?????? ?Final model choice- trainControl(method = "none", classProbs = TRUE)

set.seed(42)
FinalModel <- caret::train(PONV ~ ., data = train, 
                           method = "rf", 
                           trControl = finalControl, 
                           tuneGrid = data.frame(mtry=3),
                           metric = "ROC")

# 3. ????Accuracy??

# 3-1Model performance

# 3-1. Training modelict(FinalModel, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2) ?×½?Æ® ?????? ????. Test modelinalModel, test)
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes")
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$PONV)


# 3-3) ROC

require(Epi)
requ.re(pROC)

predictedProbs <- predict(FinalModel, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = PONV ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(PONV ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1
library(ROCR)
library(ep