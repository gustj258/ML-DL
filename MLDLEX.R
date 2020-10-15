install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
library(datasets)
data(Groceries)
itemFrequencyPlot(Groceries, topN = 20, type = "absolute")

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
summary(rules)

plot(rules)
