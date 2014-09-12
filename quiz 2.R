#1)

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y~x))


#3
data(mtcars)
fit <- lm(mpg~wt,data=mtcars)
predict(fit,data.frame(wt=mean(mtcars$wt)),interval="confidence")
summary(lm(mpg~wt,data=mtcars))
confint(lm(mpg~wt,data=mtcars))


#5
coef(fit)[1]+3*coef(fit)[2]
predict(fit,newdata=data.frame(wt=3),interval="predict")
(coef(fit)[1]+c(-1,1)*qt(.975,30,lower.tail = T)*summary(fit)$coefficients[1,2])+ 3*(coef(fit)[2]+c(-1,1)*qt(.975,30,lower.tail = T)*summary(fit)$coefficients[2,2])


#6
2*(coef(fit)[2]+c(-1,1)*qt(.975,30,lower.tail = T)*summary(fit)$coefficients[2,2])

#9
fit1 <- lm(mpg~1,data=mtcars)

sum((fit1$residuals^2))/sum((fit$residuals^2))
