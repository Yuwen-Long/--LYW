library(ggplot2)
library(patchwork)
library(glmtrans)
library(caret)
library(glmnet)
library(pROC)
library(dplyr)
library(RSBID)
load("/home/longyuwen/Data_visualization/final/cleaned_data.RData")

set.seed(42)

prop.table(table(Data$booking_bool))

df1 <- Data[Data$click_bool==1, -15]
y <- df1$booking_bool
y <- sapply(y, function(x) return(ifelse(x==1, "yes", "no")))

train_index <- createDataPartition(y, p=0.8, list=FALSE)
test_index <- setdiff(1:length(y), train_index)

X <- model.matrix(booking_bool~., df1)
X <- X[, -1]

for (col in colnames(X))
{
    if (length(unique(X[, col]))>2)
    {
        if (sd(X[, col])!=0)
        {
            X[, col] <- scale(X[, col])
        }
    }
}

p <- dim(X)[2]
p

imbal_train <- bind_cols(X[train_index,], factor(as.character(y[train_index]), levels=c("no", "yes")))
colnames(imbal_train)[p+1] <- "booking_bool"
imbal_train <- as.data.frame(imbal_train)
test <- bind_cols(X[test_index,], factor(as.character(y[test_index]), levels=c("no", "yes")))
colnames(test)[p+1] <- "booking_bool"
test <- as.data.frame(test)
for (col in colnames(X)[1:p])
{
    if (length(unique(X[, col]))<=2)
    {
        imbal_train[, col] <- factor(sapply(imbal_train[, col], as.character) , levels=c("0", "1"))
        test[, col] <- factor(sapply(test[, col], as.character) , levels=c("0", "1"))
    }
}

bal_train <- SMOTE_NC(imbal_train, "booking_bool", 200)
bal_train$booking_bool <- factor(sapply(bal_train$booking_bool, function(x) return(ifelse(x=="yes", 1, 0))))
test$booking_bool <- factor(sapply(test$booking_bool, function(x) return(ifelse(x=="yes", 1, 0))))

train_X <- 1.0 * model.matrix(booking_bool~., bal_train)
train_X <- train_X[, -1]
test_X <- 1.0 * model.matrix(booking_bool~., test)
test_X <- test_X[, -1]

real.class <- unlist(test[, p+1])

prop.table(table(bal_train$booking_bool))
prop.table(table(test$booking_bool))

# lasso_logistics
lasso_logis <- cv.glmnet(train_X, bal_train$booking_bool, family="binomial", standardize=FALSE)
lasso_logis.pred <- predict(lasso_logis, test_X)
lasso_logis.class <- sapply(lasso_logis.pred, function(x) return(ifelse(x>=0.5, 1, 0)))

table(lasso_logis.class, real.class)
lasso_logis.accuracy <- mean(real.class == lasso_logis.class)
lasso_logis.accuracy

lasso_logis.precision <- 179 / (1000+179)
lasso_logis.precision

lasso_logis.sensitivity <- 179 / (94+179)
lasso_logis.sensitivity

lasso_logis.specificity <- 1036 / (1036+1000)
lasso_logis.specificity

lasso_logis.auc <- auc(response=real.class, predictor=lasso_logis.class)
lasso_logis.auc

lasso_logis.coef <- predict(lasso_logis, type="coefficients")
col <- colnames(train_X)
for (i in 2:(p+1))
{
    if (abs(lasso_logis.coef[i]) != 0)
    {
        print(paste(col[i-1], lasso_logis.coef[i]))
    }
}