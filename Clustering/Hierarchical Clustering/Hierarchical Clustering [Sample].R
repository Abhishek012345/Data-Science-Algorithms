getwd()
input <- read.csv("Universities_Clustering.csv")
View(input)
mydata <- input[,(2:7)]
View(mydata)
normalize <- scale(mydata)
mydata
d <- dist(normalize, method = "euclidean")
fit <- hclust(d, method = "complete")
fit
plot(fit)
plot(fit, hang = -1)
groups <- cutree(fit, k=3)
rect.hclust(fit, k=3, border = "red")
member <- as.matrix(groups)
final <- data.frame(input, member)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
write.csv(final1, file="final.csv")
getwd()
