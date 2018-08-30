# Catherine Beazley
# Titanic Kaggle Competition
# SYS 6018 Data Mining
# 8/28/2018

library(tidyverse)

# reading in the files
setwd('C:/Users/cathe/Downloads/all')
train.set <- read_csv('train.csv')
test.set <- read_csv('test.csv')

# setting categorical variables to factors
train.set$Survived <- factor(train.set$Survived)
train.set$Pclass <- factor(train.set$Pclass)
train.set$Sex <- factor(train.set$Sex)
train.set$Embarked <- factor(train.set$Embarked)

test.set$Survived <- factor(test.set$Survived)
test.set$Pclass <- factor(test.set$Pclass)
test.set$Sex <- factor(test.set$Sex)
test.set$Embarked <- factor(test.set$Embarked)

# cross validating
sub <- sample(1:891, size=891/2)
train <- train.set[sub,]
valid <- train.set[-sub,]

# Includes all variables except ID, Name, ticket, and Cabin
train1 <- glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, family = "binomial")
summary(train1)

# Call:
#   glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
#         Fare + Embarked, family = "binomial", data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0408  -0.6010  -0.2873   0.5674   2.5743  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  5.860594   0.882894   6.638 3.18e-11 ***
#   Pclass2     -1.633895   0.495479  -3.298 0.000975 ***
#   Pclass3     -3.235368   0.535261  -6.044 1.50e-09 ***
#   Sexmale     -2.825474   0.354555  -7.969 1.60e-15 ***
#   Age         -0.042262   0.012872  -3.283 0.001026 ** 
#   SibSp       -0.583842   0.230224  -2.536 0.011213 *  
#   Parch        0.039343   0.202957   0.194 0.846294    
# Fare        -0.001150   0.003938  -0.292 0.770261    
# EmbarkedQ   -1.544336   1.004684  -1.537 0.124260    
# EmbarkedS   -1.165256   0.404522  -2.881 0.003970 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 471.20  on 345  degrees of freedom
# Residual deviance: 282.39  on 336  degrees of freedom
# (99 observations deleted due to missingness)
# AIC: 302.39
# 
# Number of Fisher Scoring iterations: 5

correct <- rep(0,100)
for(i in 1:100){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# Approx 70% correct


# Based on above, it seems like class, sex, and age are most significant. 
# However, if I repeat the regression many times, every so often, Embarked or SibSp are somewhat significant.

# Includes variables Pclass, Sex, Age, SibSp, and Embarked
train2 <- glm(Survived~Pclass+Sex+Age+SibSp+Embarked, data=train, family = "binomial")
summary(train2)

# Call:
#   glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Embarked, 
#       family = "binomial", data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0753  -0.6273  -0.3757   0.5826   2.4384  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.57115    0.67167   6.806 1.01e-11 ***
#   Pclass2     -1.18098    0.43557  -2.711   0.0067 ** 
#   Pclass3     -2.49547    0.44032  -5.667 1.45e-08 ***
#   Sexmale     -2.73443    0.32226  -8.485  < 2e-16 ***
#   Age         -0.05082    0.01165  -4.361 1.30e-05 ***
#   SibSp       -0.42823    0.17558  -2.439   0.0147 *  
#   EmbarkedQ   -0.47651    0.81923  -0.582   0.5608    
# EmbarkedS   -0.27947    0.38516  -0.726   0.4681    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 467.70  on 352  degrees of freedom
# Residual deviance: 302.92  on 345  degrees of freedom
# (92 observations deleted due to missingness)
# AIC: 318.92
# 
# Number of Fisher Scoring iterations: 5

correct <- rep(0,100)
for(i in 1:100){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass+Sex+Age+SibSp+Embarked, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 71% correct

# Embarked wasn't significant.

# Including variables Pclass, Sex, Age, and SibSp
train3 <- glm(Survived~Pclass+Sex+Age+SibSp, data=train, family = "binomial")
summary(train3)

# Call:
#   glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", 
#       data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.5845  -0.6326  -0.4519   0.6630   2.2586  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.75936    0.58582   6.417 1.39e-10 ***
#   Pclass2     -1.23889    0.39035  -3.174   0.0015 ** 
#   Pclass3     -2.29332    0.37853  -6.058 1.37e-09 ***
#   Sexmale     -2.44667    0.30650  -7.983 1.43e-15 ***
#   Age         -0.03308    0.01103  -3.000   0.0027 ** 
#   SibSp       -0.38943    0.16632  -2.341   0.0192 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 478.37  on 353  degrees of freedom
# Residual deviance: 333.96  on 348  degrees of freedom
# (91 observations deleted due to missingness)
# AIC: 345.96
# 
# Number of Fisher Scoring iterations: 4

correct <- rep(0,100)
for(i in 1:100){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass+Sex+Age+SibSp, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 72% correct

# Class, sex, age, and number of siblings seem to be the most significant, with class and sex the most significant.
# Now checking how combinations of these variables perform in predicting the test set in cross validation.

# Including Class, Sex, and Age
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass+Sex+Age, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 70% accuracy

# including just class
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 50%

# including just sex
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Sex, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 73%

# including class and sex
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass+Sex, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 73%

# including class and age 
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass+Age, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 56%

# including sex and age 
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Sex+Age, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 69%

# including class, sex, sibsp
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Pclass+Sex+SibSp, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 74% correct

# including sex and num siblings
correct <- rep(0,10)
for(i in 1:10){
  sub <- sample(1:891, size=891/2)
  train <- train.set[sub,]
  valid <- train.set[-sub,]
  
  train2 <- glm(Survived~Sex+SibSp, data=train, family = "binomial")
  summary(train2)
  
  probs<-as.vector(predict(train2,newdata=valid, type="response"))
  preds <- rep(0,891/2)  
  preds[probs>0.5] <- 1 
  tab <- table(preds,valid$Survived)
  correct[i] <- 1 - (tab[1,2]+tab[2,1])/(tab[1,1]+tab[2,2])
  
}
mean(correct)
# approx 73%

# Class, Sex, and number of siblings seem to make the most correct predictions

# Creating a CSV file of predictions with passenger ID and prediction of whether the passenger survived using a log regression
# model that includes Pclass, Sex, and SibSp
train.final <- glm(Survived~Pclass+Sex+SibSp, data=train.set, family = "binomial")
probs<-as.vector(predict(train.final,newdata=test.set, type="response"))
preds <- rep(0,418)  
preds[probs>0.5] <- 1 
predicts <- as.data.frame(cbind(test.set$PassengerId, preds))
write.table(predicts, file = "TitanicSurvivalPredictions.csv", row.names=F, col.names=c("PassengerID", "Survived"), sep=",")

