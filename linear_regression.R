install.packages("tidyverse")
install.packages("MASS")
install.packages("ISLR")
library(tidyverse)
library(MASS)
library(ISLR)

df <- Boston
attach(df)

df
?Boston
names(df)

lm_fit <- lm(medv~lstat, data=df)
lm_fit  

summary(lm_fit)
plot(lm_fit)

names(lm_fit)
lm_fit$fitted.values


confint(lm_fit,level=0.99) #confidence interval for PARAMETERS

predict(lm_fit, data.frame(lstat=15), interval="confidence" )

predict(lm_fit, data.frame(lstat=15), interval="prediction" )

plot(df$lstat,df$medv)

abline(lm_fit,col="red")

abline(lm_fit, lwd=3, col="red")

plot(lstat, medv, col="blue", pch=20)

par(mfrow=c(1,1))
plot(lm_fit)


#LEVERAGE STAT
plot(hatvalues(lm_fit))
plot(lm_fit$fitted.values)


#Multiple Linear Regression
multi_lm_fit <- lm(medv~., data=df)

summary(multi_lm_fit)

#for vif
install.packages("car")
library(car)

vif(multi_lm_fit)

multi_lm_fit1 <- lm(medv~.+lstat:age, data=df)

summary(multi_lm_fit1)


#non_linear transformations
poly_lm_fit <- lm(medv~lstat + I(lstat^2), data=df)
summary(poly_lm_fit)

anova(lm_fit,poly_lm_fit)

plot(poly_lm_fit)

poly_lm_fit2 <- lm(medv~log(rm),data=df)
summary(poly_lm_fit2)
plot(poly_lm_fit2)

plot(rstudent(multi_lm_fit))


##Qualitative

fix(Carseats)
df2 <- Carseats
names(df2)


?Carseats

c_lm_fit <- lm(Sales ~.+Income:Advertising-US-Urban-Population, data = df2)
summary(c_lm_fit)
contrasts(Carseats$ShelveLoc)
