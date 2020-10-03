library('mlogit')
library('nnet')

data(Mode)
head(Mode)
tail(Mode)
data()
table(Mode$choice)
Mode.choice <- multinom(choice ~ cost.car + cost.carpool + cost.bus + cost.rail + time.car +  time.carpool + time.bus + time.rail, data = Mode)
summary(Mode.choice)

z <- summary(Mode.choice)$coefficients  / summary(Mode.choice)$standard.errors
z
p_value <- (1-pnorm(abs(z),0,1))*2
p_value
?abs

exp(coef(Mode.choice))

prob <- fitted(Mode.choice)
prob
class(prob)
prob <-data.frame(prob)
prob

prob["pred"] <- NULL
prob

get_names <- function(i){
  return(names(which.max(i)))
}

pred_names <- apply(prob, 1, get_names)
prob$pred <- pred_names
View(prob$pred)

table(pred_names, Mode$choice)

mean(pred_names == Mode$choice)





