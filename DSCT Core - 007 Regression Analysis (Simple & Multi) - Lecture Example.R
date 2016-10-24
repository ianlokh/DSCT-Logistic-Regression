# require libraries
require(dplyr)
require(gplots)
require(RColorBrewer)

# load libraries
library(dplyr)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(MASS)

data(diamonds)

set.seed(58234)

# reduce the sample dataset
diamond <- diamonds[(diamonds$price > 1) & (diamonds$price < 3500) & diamonds$carat < 1.0,]
diamond <- diamond[sample(nrow(diamond), 600), ]
diamond <- diamond[complete.cases(diamond),]


ggplot(diamond, aes(x = carat, y = price)) +
  xlab("Mass (carats)") +
  ylab("Price (SIN $)") +
  geom_point(size = 3, colour = "black", alpha=0.5) +
  geom_point(size = 2, colour = "blue", alpha=0.2) +
  geom_smooth(method = "lm", colour = "black")


# rought plot of all paris of data
pairs(diamond)


# simple linear regression model - 1 factor / predictor
fit <- lm(price ~ carat, data = diamond)
summary(fit)


# multiple regression model - multi-factor / predictors
fit1 <- lm(price ~ ., data = diamond)
summary(fit1)



# using model.matrix to transform categorical to dichotomous variables
cutMatrix <- model.matrix(~ cut - 1, data=diamond)
colorMatrix <- model.matrix(~ color - 1, data=diamond)
clarityMatrix <- model.matrix(~ clarity - 1, data=diamond)


# add them into the data set
diamond <- cbind(diamond, cutMatrix, colorMatrix, clarityMatrix)


# multiple regression model - multi-factor / predictors
fit2 <- lm(price ~ . -cut -color -clarity -cutFair, data = diamond)
summary(fit2)


# using step function to step through the predictors
fit2Formula <- formula(fit2)
step.model = step(fit2, direction='both', scope=fit2Formula)
summary(step.model)


# using function stepAIC to determine the best combination of predictors
step <- stepAIC(fit2, direction="both")
step$anova # display results
summary(step)


# confidence interval of the model
confint(step, 'carat', level=0.95)
# cost of the diamond is estimated to increase between 5225.51 and 7144.956 for every increase in 1 caret


# alternate to Variance Inflationary Factors, we can use the function colldiag
library(perturb)
print(colldiag(step),fuzz=.3)
