library(plyr)
x <- runif(50)
x
y <- runif(50)
y
data <- cbind(x,y)
data
plot(data)
text(data, row.names(data))
km <- kmeans(data, 4)
str(km)
km <- kmeans.ani(data, 4)

# Apply k-Means for University Data set
input <- read.csv("Universities_Clustering.csv")
input
mydata <- input[,c(2:7)]
mydata
normalize <- scale(mydata[,1:6])
normalize
fit <- kmeans(normalize, 5)
str(fit)

final1 <- data.frame(mydata, fit$cluster)
final1
final2 <- final1[,c(ncol(final1),1:(ncol(final1)-1))]
final2




