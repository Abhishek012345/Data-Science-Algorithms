View(wbcd)
wbcd <- wbcd[-1]
View(wbcd)
table(wbcd$diagnosis)
str(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
View(wbcd)
str(wbcd)
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
normalize <- function(x){
  return(x-min(x) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
View(wbcd_n)
summary(wbcd_n$area_mean)

