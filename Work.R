install.packages("caTools")
rm(list = ls())
library(caTools)
library("ISLR")
library(caret)

dataset <-read.csv("50_Startups.csv")

#create the dummy variables
dummy_matrix <- model.matrix(~ State, data = dataset)
dataset <- cbind(dataset[, -4], Florida = dummy_matrix[,2], NY= dummy_matrix[,3])


set.seed(123)
split = sample.split(dataset$Profit, 0.8)
training <- dataset[split, ]
testing <- dataset[!split,]

## Feature plot (*caret* package)
featurePlot(x=training[,c("Administration","Marketing.Spend", "R.D.Spend")],
            y = training$Profit,
            plot="pairs")
cor(training[,-4])

reg <- lm(formula = Profit ~., data = training)
reg <- lm(formula = Profit ~.- NY, data = training)

reg <- lm(formula = Profit ~ R.D.Spend + Administration+ Marketing.Spend, data = training)
# R.D.Spend        7.983e-01  5.356e-02  14.905  < 2e-16 ***
# Administration  -2.895e-02  5.603e-02  -0.517    0.609    
# Marketing.Spend  3.283e-02  1.987e-02   1.652    0.107    
# Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9457 
reg <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = training)
# R.D.Spend       7.879e-01  4.916e-02  16.026   <2e-16 ***
# Marketing.Spend 3.538e-02  1.905e-02   1.857   0.0713 .  
# Multiple R-squared:  0.9495,	Adjusted R-squared:  0.9468 
reg <- lm(formula = Profit ~ R.D.Spend, data = training)
# R.D.Spend   8.563e-01  3.357e-02   25.51   <2e-16 ***
# Multiple R-squared:  0.9448,	Adjusted R-squared:  0.9434 
reg <- lm(formula = Profit ~ poly(R.D.Spend,5), data = training)
# poly(R.D.Spend, 5)1   250924       9063  27.687   <2e-16 ***
# poly(R.D.Spend, 5)2    -4507       9063  -0.497   0.6222    
# poly(R.D.Spend, 5)3    23909       9063   2.638   0.0125 *  
# poly(R.D.Spend, 5)4   -10881       9063  -1.201   0.2382    
# poly(R.D.Spend, 5)5    13159       9063   1.452   0.1557    
# Multiple R-squared:  0.9581,	Adjusted R-squared:  0.9519 

#final model
reg <- lm(formula = Profit ~ poly(R.D.Spend,6)+ Marketing.Spend, data = training)
# poly(R.D.Spend, 6)1  2.365e+05  1.371e+04  17.249   <2e-16 ***
# poly(R.D.Spend, 6)2 -5.478e+03  8.831e+03  -0.620   0.5394    
# poly(R.D.Spend, 6)3  2.094e+04  9.063e+03   2.311   0.0275 *  
# poly(R.D.Spend, 6)4 -1.179e+04  8.827e+03  -1.336   0.1911    
# poly(R.D.Spend, 6)5  1.085e+04  8.962e+03   1.210   0.2350    
# poly(R.D.Spend, 6)6 -1.352e+04  8.813e+03  -1.534   0.1349    
# Marketing.Spend      2.547e-02  1.855e-02   1.373   0.1792    
# Residual standard error: 8802 on 32 degrees of freedom
# Multiple R-squared:  0.9628,	Adjusted R-squared:  0.9547 
# F-statistic: 118.3 on 7 and 32 DF,  p-value: < 2.2e-16

reg <- lm(formula = Profit ~ poly(R.D.Spend,6)+ poly(Marketing.Spend, 5), data = training)
# poly(R.D.Spend, 6)1       252087.0    29074.3   8.670 2.03e-09 ***
# poly(R.D.Spend, 6)2       -11000.2    31729.5  -0.347   0.7314    
# poly(R.D.Spend, 6)3        26226.8    26579.9   0.987   0.3322    
# poly(R.D.Spend, 6)4       -17888.2    16596.0  -1.078   0.2903    
# poly(R.D.Spend, 6)5         1687.4    12695.1   0.133   0.8952    
# poly(R.D.Spend, 6)6       -30404.3    11760.8  -2.585   0.0152 *  
# poly(Marketing.Spend, 5)1   8931.9    29444.8   0.303   0.7639    
# poly(Marketing.Spend, 5)2    571.1    34496.4   0.017   0.9869    
# poly(Marketing.Spend, 5)3 -10813.1    26012.2  -0.416   0.6808    
# poly(Marketing.Spend, 5)4  37293.2    15479.5   2.409   0.0228 *  
# poly(Marketing.Spend, 5)5  -6617.8    10277.0  -0.644   0.5249  
# Residual standard error: 7389 on 28 degrees of freedom
# Multiple R-squared:  0.9771,	Adjusted R-squared:  0.968 
# F-statistic: 108.4 on 11 and 28 DF,  p-value: < 2.2e-16

y <- testing$Profit
n <- length(y)

y_hat1 <- predict(reg, newdata = testing)
RSS1 <- sum((y - y_hat1) * (y - y_hat1))
RMSE1 <- sqrt(RSS1/n)

#Support vector machine
library(e1071)
reg <- svm(formula = Profit ~ ., data = training, type = "eps-regression")
summary(reg)
y_hat2 <- predict(reg, newdata = testing)
RSS2 <- sum((y - y_hat2) * (y - y_hat2))
RMSE2 <- sqrt(RSS2/n)
compare_res <- cbind(y,y_hat1)
compare_res <- cbind(compare_res,y_hat2)
compare_res <- cbind(compare_res, delta_hat1 = compare_res[,1]-compare_res[,2], delta_hat2 = compare_res[,1]-compare_res[,3])
compare_res

#visualisation of information
summary(reg)
ggplot()+
  geom_point(aes(x = dataset$R.D.Spend, y = dataset$Profit), 
             colour = "red")+
  geom_point(aes(x = dataset$R.D.Spend, y = predict(reg, newdata = dataset)), 
            colour = "blue")

#ToDo add forest and random forest examples


# ==========================
#Next Step Logical Regression
rm(list = ls())
dataset <-read.csv("Social_Network_Ads.csv")
names(dataset)

#create the dummy variables
dummy_matrix <- model.matrix(~ Gender, data = dataset)
dataset <- cbind(subset(dataset, select = c(-1, -2)), isMale = dummy_matrix[,2])
dataset$Purchased <- as.factor(dataset$Purchased)

set.seed(123)
split = sample.split(dataset$Purchased, 0.75)
training <- dataset[split, ]
testing <- dataset[!split,]

## Feature plot (*caret* package)
featurePlot(x=training[,c("Age","EstimatedSalary", "isMale")],
            y = training$Purchased,
            plot="pairs")

## Add regression smoothers
col1 <- as.numeric(training$Purchased)+1
ggplot(training, aes(x = Age,y = EstimatedSalary))+
  geom_point(colour = col1)+
  geom_smooth(method='lm',formula=y~x)

col2 <- as.numeric(training$isMale)+1
ggplot(training, aes(x = Age,y = EstimatedSalary))+
  geom_point(colour = col2)

modelFit <- glm(formula = Purchased ~ ., 
           family = binomial , 
           training)
# Age              2.660e-01  3.384e-02   7.861 3.82e-15 ***
# EstimatedSalary  4.159e-05  6.685e-06   6.221 4.94e-10 ***
# isMale1          3.449e-01  3.525e-01   0.979    0.328    
# Null deviance: 416.79  on 319  degrees of freedom
# Residual deviance: 208.88  on 316  degrees of freedom
# AIC: 216.88
modelFit1 <- glm(formula = Purchased ~ . - isMale, 
           family = binomial , 
           training)
# Age              2.620e-01  3.322e-02   7.885 3.14e-15 ***
# EstimatedSalary  4.096e-05  6.619e-06   6.189 6.04e-10 ***
# Null deviance: 416.79  on 319  degrees of freedom
# Residual deviance: 209.84  on 317  degrees of freedom
# AIC: 215.84

summary(modelFit1)
pred1 <- predict(modelFit1, type = "response", newdata = testing[-3])
y_hat <- ifelse(pred1>0.5, 1, 0)
y_hat <- as.factor(y_hat)

library(caret)
modelFit2 <- train(Purchased ~.- isMale,data=training, method="glm")
pred2 <- predict(modelFit2, newdata = testing)

confusionMatrix(testing$Purchased, y_hat)
confusionMatrix(testing$Purchased, pred2)
