
library(MASS)
data(shuttle)

#1
fit1 <- glm(use ~ wind,data=shuttle,family="binomial")
summary(fit1)
exp(fit1$coef)

#2
fit2 <- glm(use ~ wind + magn,data=shuttle,family="binomial")
summary(fit2)
exp(fit2$coef)

#4
data(InsectSprays)
fit4 <- glm(count ~ spray,data=InsectSprays,family="poisson")
summary(fit4)
exp(summary(fit4)$coef)
summary(fit4)$coeff[1]/(summary(fit4)$coeff[1]+summary(fit4)$coeff[2])
