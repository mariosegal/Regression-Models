
library(MASS)
data(shuttle)

#1
fit1 <- glm(use ~ wind,data=shuttle,family="binomial")
summary(fit1)

#2
fit2 <- glm(use ~ wind + magn,data=shuttle,family="binomial")
summary(fit2)


#4
data(InsectSprays)
fit4 <- glm(count ~ spray,data=InsectSprays,family="poisson")
summary(fit4)
