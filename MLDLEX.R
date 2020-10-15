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
inspect(rules[1:5])

rules <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(rules[1:5])

set.seed(0)
x <- rbinom(1000, 10, 0.3)
hist(x)
mean(x)
var(x)

rx = rpois(3000, 2)
mean(rx)
var(rx)
hist(rx, probability=TRUE)
rnorm(1, 100, 16)
x=rnorm(100)
hist(x, probability=TRUE)
curve(dnorm(x), add=T)
x=rt(100, df=3)
hist(x, probability=TRUE)
curve(dt(x, 3), add=T)

x=rchisq(100, 1)
hist(x, probability=TRUE)
curve(dchisq(x, 1), add=T)

x <- rnorm(100, mean=0)
shapiro.test(x)

x1 <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
x2 <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)

t.test(x1, x2, paired=TRUE, conf.level=0.95)

freq = c(22, 21, 22, 27 ,22, 36)   # 주사위를 150번 던졌을 때 눈별로 나온 빈도
probs = c(1, 1, 1, 1, 1, 1)/6      # 이론적으로 각 눈은 동일한 확률로 나오는 것을 표현

chisq.test(freq, p=probs)

install.packages("corrplot")
library(corrplot)
data(mtcars)
mtcars.cor = cor(mtcars)

corrplot(mtcars.cor, method="circle")
corrplot.mixed(mtcars.cor)

x1 = 1:10
x2 = 10:1

cor.test(x1, x2)

data(InsectSprays)
attach(InsectSprays)
str(InsectSprays)

mean(InsectSprays$count)
var(InsectSprays$count)
table(InsectSprays$spray)
mean(InsectSprays[InsectSprays$spray=="A", 1])

attach(InsectSprays)

oneway.test(count~spray, var.equal=TRUE)
aov.out = aov(count~spray, data=InsectSprays)
summary(aov.out)

pairwise.t.test(count, spray, p.adjust="bonferroni")
TukeyHSD(aov.out)
