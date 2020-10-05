# Libraries Needed
library(caret)
library(glmnet)
install.packages("mlbench")
library(mlbench)
library(psych)

# Data
data("BostonHousing")
data <- BostonHousing
View(data)
?BostonHousing
pairs.panels(data[c(-4,-14)],cex=2)#scatter plot for every possible combination of independent variables

# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

# Linear Model
set.seed(1234)
lm <- train(medv ~., train, method = 'lm', trControl = custom)

# Results
lm$results
lm
summary(lm)
plot(lm$finalModel)

# Ridge Regression
set.seed(1234)
ridge <- train(medv ~ ., train, method ='glmnet', tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 5)), trControl = custom)
#lambda is a hyperparameter is estimated using crossvalidation,it is basically strength of penalty on coefficients
#as we increase lambda , we are increasing the penalty and therefore make coefficients to shrink
# Plot Results
plot(ridge)#y axis we have lambda, after lambda 0.5 your rmse value increases
ridge
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))

# Lasso Regression
set.seed(1234)
lasso <- train(medv ~ ., train, method ='glmnet', tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 5)), trControl = custom)

# Plot Results
plot(lasso)
lasso
plot(lasso$finalModel, xvar = 'lambda', label=T)
plot(lasso$finalModel, xvar = 'dev', label=T)
plot(varImp(lasso, scale=T))

# Elastic Net Regression
set.seed(1234)
en <- train(medv ~ ., train, method ='glmnet', tuneGrid = expand.grid(alpha = seq(0, 1, length = 10), lambda = seq(0.0001, 1, length = 5)), trControl = custom)

# Plot Results
plot(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en))

# Compare Models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = en)
res <- resamples(model_list)#resamples is part of caret package, which will help us compare
summary(res)

# Best Model
lasso$bestTune
best <- lasso$finalModel
coef(best, s = lasso$bestTune$lambda)

# Save Final Model for Later Use
saveRDS(lasso, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

# Prediction
p1 <- predict(fm, train)
sqrt(mean((train$medv-p1)^2))

p2 <- predict(fm, test)
sqrt(mean((test$medv-p2)^2))
