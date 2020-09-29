install.packages("adabag")
library(adabag)
library(caret)

indexes=createDataPartition(iris$Species, p=.90, list = F)
train = iris[indexes, ]
test = iris[-indexes, ]

model = boosting(Species~., data=train, boos=TRUE, mfinal=25)
print(names(model))
print(model$trees[2])

pred = predict(model, test)
pred
print(pred$confusion)
print(pred$error)

result = data.frame(test$Species, pred$prob, pred$class)
print(result)
