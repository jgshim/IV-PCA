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

train2 <- smote(PONV~., train, perc.over = 2, perc.under=2)
table(train2$PONV)

data_train <- train2
data_test <- test

#########################

### Keras model

library(tensorflow)
library(ggplot2)
library(caTools)

str(data_train$PONV)
class(data_train$PONV)
data_train$PONV <- as.numeric(data_train$PONV)
table(data_train$PONV)
data_train$PONV <- ifelse(data_train$PONV==1, 0, 1)
data_test$PONV <- as.numeric(data_test$PONV)
data_test$PONV <- ifelse(data_test$PONV==1, 0, 1)
str(data_train$PONV)
str(data_test$PONV)
table(data_train$PONV)
table(data_test$PONV)

input <- as.matrix(data_train[1:13], ncol = 13)
output <- as.matrix(data_train[14], ncol = 1)
input2 <- as.matrix(data_test[1:13], ncol = 13)
output2 <- as.matrix(data_test[14], ncol = 1)

#3. Setting up the random seed prior to calling train

seed <- 42
.train set / test set split

input_train <- input
input_test <- input2
output_train <- output
output_test <- output2
str(output_test)


liblry(keras)

# ????Defining model?build_model <- function() {
  modras_model_sequential() %>% 
    layer_dense(units = 26, kern32, kernel_regularizer = regularizer_l2(0.001), activation = "relu") %>% 
    layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001), activation = "relu") %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}
 ????-fold cross-validation
indices <- sam-foldsample(1:nrow(input_trcds <- cut(1:length(indices), breaks = k, labels = FALSE)

num_epochs <- 200
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- input_train[val_indices,]
  val_targets <- output_train[val_indices]
  
  partial_train_data <- input_train[-val_indices,]
  partial_train_targets <- output_train[-val_indices]
  
  model <- build_model()
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 20, verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results$accuracy)
}

all_scores
mean(all_scores)

num_epochs <- 200
all_acc_histories <- NULL
all_loss_histories <- NULL

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- input_train[val_indices,]
  val_targets <- output_train[val_indices]
  
  partial_train_data <- input_train[-val_indices,]
  partial_train_targets <- output_train[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 20, verbose = 0
  )
  acc_history <- history$metrics$val_acc
  all_acc_histories <- rbind(all_acc_histories, acc_history)
  loss_history <- history$metrics$val_loss
  all_loss_histories <- rbind(all_loss_histories, loss_history)
}

# ???? ???Gestory of the k-fold cross-validation 
poch = seq(1average_acc_history <- data.frame(
  e:ncol(all_acc_histories)),
  validation_acc = apply(all_acc_histories, 2, mean)
)
average_loss
_history <- data.frame(
  epoch = seq(1:ncol(all_loss_histories)),
  validation_loss = apply(all_loss_histories, 2, mean)
)

# ???? Á¡?? ?×Plotting validation scoreacc_hiPlotting validatstory, aes(x = epoch_acc)) + geom_line()
ggplot(average_loss_history, aes(x = epoch, y = validation_loss)) + geom_line()

# geom_smooth()?? ???? Á¡???? ?
= validation_acc)) + geom_smooth()
ggplot(average_loss_history, aes(x = epoch, y = validation_loss)) + geom_smooth()

# ??Á¾ ???? ?Æ·??Ï±?
mTraining final modelmodeTraining final model outl %>% fit(input_train,     epochs = 15, batch_size = 20, verbose = 0)
results <- model15>% evaluate(input_test %output_test)
model %>% predict(input_test) -> pred_test

class(pred_test)
class(output_test)
ANN_ROC <- dat# ---- Model save using a.frame(x = pred_test, y = output_test)

require(Epi)
require(pROC)

output_test_df <- as.data.frame(output_test)
pred_test_2, data = output_test_d plot="ROC")
b11 = r_dfPONV ~ pred_test_2, output_test_df, ci=Tpercent=T)

plot(b11)

b11
a11

a.frame(x = pred_test,