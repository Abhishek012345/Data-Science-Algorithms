install.packages("arules")
library("arules") # Used for building association rules i.e. apriori algorithm
data("Groceries")# loading the Groceries Datawere u 
Groceries

?Groceries
inspect(Groceries[1:10])# showing only top 10 transactions
install.packages("arueslViz")
library("arulesViz") # for visualizing rules

# making rules using apriori algorithm 
# Keep changing support and confidence values to obtain different rules
?apriori
# Building rules using apriori algorithm
arules <- apriori(Groceries, parameter = list(support=0.002,confidence=0.6))
arules
inspect(head(sort(arules,by="lift"))) # to view we use inspect 

# Different Ways of Visualizing Rules
plot(arules)
plot(arules,method="grouped")
plot(arules[1:30],method = "graph") # for good visualization try plotting only few rules


write(arules, file="a_rules.csv",sep=",")

#getwd()

