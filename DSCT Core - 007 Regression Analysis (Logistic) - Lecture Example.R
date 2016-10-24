# Lecture Example on Logistic Regression

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

setwd("/Users/ianlo/Work/Projects/Data Science Competency/Curriculum/00 Core/src/")

# Load the training csv file
training.data.raw <- read.csv('./titanic_train.csv',header=T,na.strings=c(""))


# Check for each variable how many have missing records
sapply(training.data.raw, function(x) sum(is.na(x)))


# Check for each variable how many unique values there are
sapply(training.data.raw, function(x) length(unique(x)))


# Using Amelia package to visualise the distribution of missing values in the data
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")


# select all other values except cabin since that has the most missing vaues
# We will also drop PassengerId since it is only an index and Ticket.
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))


# for missing values, we perform simple data imputation my using the mean age
# alternatively, we can also use median
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)


# The contrast specifies how the levels of the factors will be coded into a family
# of numeric dummy variables for fitting the model - i.e this will show us how
# the variables have been dummyfied by R and how to interpret them in a model.
contrasts(data$Sex)
contrasts(data$Embarked)

# Remove the 2 rows with missing value in Emarked column (can also inpute if desired)
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

# create a separate model training and testing set (typically 80/20)
train <- data[1:704,]
test <- data[705:889,]

# sample distribution of survived and died
table(train$Survived, train$Sex)


# create a logistic regression model with the training data
# target is Survived, predictor variables are all values
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
# review the model
summary(model)


# odds ratios and 95% CI
exp(cbind(OR = coef(model), confint(model)))

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(model, test="Chisq")

# When you run anova(my.mod, test="Chisq"), the function compares the following models in sequential order:
# glm(y~1, family="binomial") vs. glm(y~x1, family="binomial")
# glm(y~x1, family="binomial") vs. glm(y~x1+x2, family="binomial")
# glm(y~x1+x2, family="binomial") vs. glm(y~x1+x2+x3, family="binomial")
# Each of those comparisons is done via a likelihood ratio test
# http://stats.stackexchange.com/questions/59879/logistic-model-what-is-more-important-anova-chi-sq-test-or-significance-of-coe


# Analyzing the table we can see the drop in deviance when adding each variable one at a time.
# Again, adding Pclass, Sex and Age significantly reduces the residual deviance.
# The other variables seem to improve the model less even though SibSp has a low p-value.
# A large p-value here indicates that the model without the variable explains more or less
# the same amount of variation. Ultimately what you would like to see is a significant drop
# in deviance 


# use the model to predict for cases 2 - 8 in the test set
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
# the results of the model is a probability value and must be mapped to 1 or 0
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

# determine the mis-classification error
misClasificError <- mean(fitted.results != test$Survived)
# calculate the accuracy
print(paste('Accuracy',1-misClasificError))