library(dplyr)
library(reshape2)
library(ggplot2)
library(MASS)
library(ggfortify)
options(scipen=999)

setwd("C:\\Users\\yueqiteo\\Desktop\\00 Core\\Assignments\\003 Regression and Model Evaluation")
credit <- read.csv2("german_credit_data.csv", header = TRUE)
colnames(credit) <- c("account_status", "loan_duration", "credit_history", "loan_purpose",
                      "credit_amount_request", "savings_account", "employment", "installment_rate"
                      ,"marital_status_and_gender", "debtors_guarantor", "residence", 
                      "assets", "age", "installment_plans", "housing", "existing_credits",
                      "job_classification", "num_ppl_liable", "telephone", "foreign_worker", 
                      "recommendation")



# 1)	Assess the data and ensure that there are no defects in it. 
# Data cleaning and transformation
# Recoding account status to median value of the respective range 
credit[credit$account_status == 1, "account_status"] <- 0  
credit[credit$account_status == 2, "account_status"] <- 100  
credit[credit$account_status == 3, "account_status"] <- 200  
credit[credit$account_status == 4, "account_status"] <- NA
credit$account_status[is.na(credit$account_status)] <- mean(credit$account_status,na.rm=T)

# Recoding credit history to 1, 2, 3, 4, 5 instead of starting with 0
credit$c_history <- 0
credit[credit$credit_history == 0, "c_history"] <- 1
credit[credit$credit_history == 1, "c_history"] <- 2
credit[credit$credit_history == 2, "c_history"] <- 3
credit[credit$credit_history == 3, "c_history"] <- 4
credit[credit$credit_history == 4, "c_history"] <- 5
credit <- credit[-3]

# Recoding savings account to median value of the respective range
credit[credit$savings_account == 1, "savings_account"] <- 50 # Assume from 0 - 99
credit[credit$savings_account == 2, "savings_account"] <- 300
credit[credit$savings_account == 3, "savings_account"] <- 750
credit[credit$savings_account == 4, "savings_account"] <- 1000
credit[credit$savings_account == 5, "savings_account"] <- NA
credit$savings_account[is.na(credit$savings_account)] <- mean(credit$savings_account,na.rm=T)

# Recode employment period to median value of the respective range
credit[credit$employment == 2, "employment"] <- 0.5
credit[credit$employment == 3, "employment"] <- 2.5
credit[credit$employment == 4, "employment"] <- 5.5
credit[credit$employment == 5, "employment"] <- 7
credit[credit$employment == 1, "employment"] <- NA
credit$employment[is.na(credit$employment)] <- mean(credit$employment,na.rm=T)



# Q2)	Determine if any categorical variables will need to be transformed, combined or split up. 
# Recode personal status and sex to split into two variables
credit$gender <- 1 ; credit$marital <- 1
credit[credit$marital_status_and_gender == 2 | credit$marital_status_and_gender == 5, "gender"] <- 2
credit[credit$marital_status_and_gender == 1 | credit$marital_status_and_gender == 3 
       | credit$marital_status_and_gender == 4, "gender"] <- 1

credit[credit$marital_status_and_gender == 1 | credit$marital_status_and_gender == 2 | 
         credit$marital_status_and_gender == 4, "marital"] <- 1 # Married before
credit[credit$marital_status_and_gender == 3 | credit$marital_status_and_gender == 5, "marital"] <- 2 # Single
credit <- credit[-8]



# Q3)	Determine if there are any natural grouping of the credit applications using 
# clustering techniques (k-means, MDS)?
# 2 natural groupings can be witnessed for credit applications
library(ggfortify)
credit_scaled <- as.data.frame(scale(credit))
credit_scaled <- credit_scaled[-19]
km <- kmeans(credit_scaled, 2, iter.max = 25, nstart = 5, algorithm="Hartigan-Wong")
autoplot(km, data = credit_scaled)



# Q4)	Formulate and construct a suitable regression model.
# Splitting dataset into training (80%) and testing (20%) sets 
credit$recommendation <- as.factor(credit$recommendation)
train <- credit[1:805,]
test <- credit[806:1005,]

# Build a logistic regression model with the training data
# Target is recommendations by credit analyst which is 
# categorical in nature. The remaining variables are 
# used as predictors.

# The most significant two variables are loan duration and
# credit history. Credit history has the lowest P-value which
# suggests that there is a strong association of an applicant's 
# credit history with the probability of an application being 
# accepted by credit analysts.
model <- glm(recommendation ~. , family=binomial(link='logit'), data=train)
summary(model)

# Reviewing the model using ANOVA function to analyze the table
# of deviance.We are looking out for significant drop in deviance here.
# The difference between the null deviance and the 
# residual deviance shows how our model is doing against the null model 
# (a model with only the intercept). The wider this gap, the better.

# Loan duration, credit history and savings account improved the model 
# a lot more as compared to the other variables as the residual deviance reduces
# more when these three variables were being added into the model.
# A large P-value here indicates that the model is able to explain more
# or less the same amount of variation with or without that particular variable
anova(model, test="Chisq")



# Q5)	Step through your model and determine if there is any multicollinearity 
# in the predictors and adjust your model accordingly.

# Final predictors that were chosen by stepAIC are:
# account_status + loan_duration + savings_account +
# employment + assets + installment_plans + existing_credits +
# telephone + foreign_worker + c_history + marital
step <- stepAIC(model, direction="both")
step$anova # display results
summary(step)

# Installment plans, existing credits, telephone and foreign worker are not significant to the
# model. Remove them and rebuild the model.
model2 <- glm(recommendation ~ account_status + loan_duration + savings_account +
                employment + assets  + c_history + marital, 
              family=binomial(link='logit'), data=train)
summary(model2)

# No Multicollinearity
library(car)
vif(model2)

library(perturb)
print(colldiag(model2),fuzz=.3)


# Q6)	Explain the best regression model that you have constructed.
# The most significant two variables are loan duration and
# credit history. Credit history has the lowest P-value which
# suggests that there is a strong association of an applicant's 
# credit history with the probability of an application being 
# accepted by credit analysts.

table(credit$recommendation, credit$loan_duration)
# highest frequency loan duration = 12
# The coefficient of 12 months loan duration 
# 12 months loan duration = 0.830 + (0.0323 * 12) = 1.2176
# 13 months loan duration = 0.830 + (0.0323 * 13) = 1.2499
# Log odds difference = 0.0323 = expected change in log odds ratio per
# 1 unit increase in loan duration.
# Odds ratio = e ^ 0.0323 = 1.0328
# Odds of recommendation will increase by 1.0328 in relation to 
# a credit application's loan duration.



# Q7)	Assess the predictive ability of the model and 
# comment on your assessment.
results <- predict(model2, newdata = test[-19],type='response')
# Decision boundary is 0.5. When probability is more than 0.5, predicted result
# = 2, and 1 if the probability is less than 0.5.
fittedResults <- ifelse(results > 0.5, 2, 1)

# Calculate the accuracy of the model
missedClassified <- sum(fittedResults != test$recommendation)/nrow(test)
print(paste('Accuracy', 1 - missedClassified)) 
# 0.73 Accuracy
# Do note that different % used to split the dataset into train
# and test datasets will give a different results. 



# Q8)	Construct a ROC curve and calculate the AUC to evaluate 
# the regression model that you have built. 
# Is the model you have constructed effective?
library(ROCR)
predict <- prediction(results, test$recommendation)
perf <- performance(predict, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC Curve", colorize = TRUE)

# As a rule of thumb, a model with good predictive ability 
# should have an AUC closer to 1 (1 is ideal) than to 0.5.
# AUC = 0.78 hence the model was a relatively effective one.
AUC <- performance(predict, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC