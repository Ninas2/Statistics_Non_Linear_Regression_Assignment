setwd("C:\\Users\\ninas\\R\\RPackages")

.libPaths('C:\\Users\\ninas\\R\\RPackages')

#loading libraries
library(readxl)
library('psych')
library('plyr')
library('glmnet')
library(car)
library('aod')
library(MASS)

#read xls file
sale_attempts <- read_xls("C:\\Users\\ninas\\OneDrive\\Desktop\\MSc Business Analytics\\2nd Quarter\\Statistics for BA 2\\Assignment 1\\project I  2021-2022.xls")
sale_attempts <- data.frame(sale_attempts)
#check for nulls, NAs
sum(is.na(sale_attempts))
sum(is.null(sale_attempts))

#checking the shape of the data
str(sale_attempts)
describe(sale_attempts)
summary(sale_attempts)
str(sale_attempts)
#update job, marital status, education, loan default index, housing loan  index, personal loan index 
#contact index, month and weekday of last contact and campaign outcome index to factors
sale_attempts$job <- factor(sale_attempts$job)
sale_attempts$marital <- factor(sale_attempts$marital)
sale_attempts$education <- factor(sale_attempts$education)
sale_attempts$default <- factor(sale_attempts$default)
sale_attempts$housing <- factor(sale_attempts$housing)
sale_attempts$loan <- factor(sale_attempts$loan)
sale_attempts$contact <- factor(sale_attempts$contact)
sale_attempts$month <- factor(sale_attempts$month)
sale_attempts$day_of_week <- factor(sale_attempts$day_of_week)
sale_attempts$poutcome <- factor(sale_attempts$poutcome)
#update subscription index from no/yes to 0/1 since it will be the dependent variable in the model
sale_attempts[which(sale_attempts$SUBSCRIBED == 'no'),21] <- '0'
sale_attempts[which(sale_attempts$SUBSCRIBED == 'yes'),21] <- '1'
sale_attempts$SUBSCRIBED <- as.numeric(sale_attempts$SUBSCRIBED)
unique(sale_attempts$default)


#all values of 'consumer confidence index' are negative, they are updated to positive number
sale_attempts$cons.conf.idx <-  (-1)*sale_attempts$cons.conf.idx

n <- nrow(sale_attempts)
#sale_attempts[sale_attempts[,13] == 999,13] <- NA
#variable pdays has '999' in almost all of its observations, it does not make any sense to keep them like that,
#but since there is already a variable that indexes whether a customer was contacted in previous campaigns (poutcome)
#the variable pdays is dropped completely
sale_attempts <- sale_attempts[,-13]

#create a season variable based on the 'months' variable and update it to a factor
for (i in 1:n){ 
  if ((sale_attempts[i,9] == 'dec') | (sale_attempts[i,9] =='jan') | (sale_attempts[i,9] =='feb')){
    sale_attempts$season[i] <- 'Winter'
  } else  if ((sale_attempts[i,9] == 'mar') | (sale_attempts[i,9] =='apr') | (sale_attempts[i,9] =='may')) {
    sale_attempts$season[i] <- 'Spring'
  } else  if ((sale_attempts[i,9] == 'jun') | (sale_attempts[i,9] =='jul') | (sale_attempts[i,9] =='aug')) {
    sale_attempts$season[i] <- 'Summer'
  } else {
    sale_attempts$season[i] <- 'Fall'
  }
}
sale_attempts$season <- factor(sale_attempts$season)



#creation of constant model
const_model <- glm(SUBSCRIBED~1, data = sale_attempts, family = 'binomial')
summary(const_model)

#creation of a full model
full_model <- glm(SUBSCRIBED~., data = sale_attempts, family = 'binomial')
summary(full_model)

#creation of different lambdas that will be used in the cross-validation lasso process
lambdas <- 10 ^ seq(8,-4,length=500)
#creation of a model matrix of the data
x_matrix <- model.matrix(full_model)[,-1]; 
fit_lasso <- glmnet(x_matrix, sale_attempts$SUBSCRIBED, alpha=1, lambda=lambdas, family="binomial")

#cross-validation lasso
lasso.cv <- cv.glmnet(x_matrix, sale_attempts$SUBSCRIBED, alpha=1,lambda=lambdas
                      , family="binomial",type.measure='class')
coef(lasso.cv, s = "lambda.1se")
coef(lasso.cv, s = "lambda.min")
#min and 1-standard error lambda values
lasso.cv$lambda.min
lasso.cv$lambda.1se
#plotting of lasso process
plot(lasso.cv)
plot(lasso.cv$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso.cv$lambda.min, lasso.cv$lambda.1se)), lty =2)


#age, previous, cons.conf.idx , euribor3m are dropped from the lasso process
lasso_model <- update(full_model, . ~ . -age -previous -cons.conf.idx -euribor3m)
summary(lasso_model)


#use bic to create a good model for inference - all of them produce the same model
step_model1 = step(lasso_model, direction = 'backward', k = log(n))
summary(step_model1)
step_model2 = step(const_model, scope=list(lower=const_model,upper=lasso_model), direction = 'both', k = log(n))
summary(step_model2) #for now we will move on with this model
step_model3 = step(const_model, scope=list(lower=const_model,upper=lasso_model), direction = 'forward', k = log(n))
summary(step_model3)

#check for multicollinearity - some variables have a multi-coll. problem
round(vif(step_model2),2)

final_model <- update(step_model2, . ~ . - emp.var.rate) #removal of emp.var.rate
summary(final_model)
round(vif(final_model),2) # issue resolved


round(final_model$coefficients,4)
#Hypothesis testing for the variables
#test for months
wald.test(b = coef(step_model2), Sigma = vcov(step_model2), Terms = 4:12)

#test for poutcome
wald.test(b = coef(step_model2), Sigma = vcov(step_model2), Terms = 13:14)

#test for credit in default
wald.test(b = coef(step_model2), Sigma = vcov(step_model2), Terms = 15:16)

#drop of the cons.price.index since it does not have a significant effect for the model
final_model <- update(final_model, . ~ . - cons.price.idx ) #removal of cons.price.idx 
summary(final_model)


#Diagnostics of the model
#deviance from the null model
with(final_model, null.deviance - deviance)
with(final_model, df.null - df.residual)
with(final_model, pchisq(null.deviance - deviance,df.null - df.residual, lower.tail = FALSE))
#significant difference between the final and the constant model

#deviance from the 'saturated' model
with(final_model, pchisq(deviance, df.residual, lower.tail = FALSE))
#not reject that the model fits 'well'

#plotting of deviance residuals
var_list = c(5, 8, 9, 11, 12, 14, 19)
var_names = c('Credit Default', 'Contact Type', 'Month', 'Call Duration', 'Campaign Calls'
              ,'Previous Campaign Outcome', 'Number of Employees Indicator')
par(mfrow = c(2,4))
for (i in 1:length(var_list)){
  if (i == 1){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'red', cex = 1, xaxt = "n")
    axis(1, at=1:3, labels=c('No', 'Unknown', 'Yes'))
  } else if (i == 2){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'red', cex = 1, xaxt = "n")
    axis(1, at=1:2, labels=c('Cellular', 'Telephone'))
  } else if (i ==3){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'red', cex = 1, xaxt = "n")
    axis(1, at=1:10, labels=c('Apr','Aug', 'Dec', 'Jul', 'Jun', 'Mar', 'May', 'Nov', 'Oct', 'Sep'))
  } else if (i == 6){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'red', cex = 1, xaxt = "n")
    axis(1, at=1:3, labels=c('Failure', 'Non-Existent', 'Success'))
  } else {
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'red', cex = 1)
  }
  
}

#plotting of pearson residuals
par(mfrow = c(2,4))
for (i in 1:length(var_list)){
  if (i == 1){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'blue', cex = 1, xaxt = "n", ylim = c(-2,2))
    axis(1, at=1:3, labels=c('No', 'Unknown', 'Yes'))
  } else if (i == 2){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'blue', cex = 1, xaxt = "n", ylim = c(-2,2))
    axis(1, at=1:2, labels=c('Cellular', 'Telephone'))
  } else if (i ==3){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'blue', cex = 1, xaxt = "n", ylim = c(-2,2))
    axis(1, at=1:10, labels=c('Apr','Aug', 'Dec', 'Jul', 'Jun', 'Mar', 'May', 'Nov', 'Oct', 'Sep'))
  } else if (i == 6){
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'blue', cex = 1, xaxt = "n", ylim = c(-2,2))
    axis(1, at=1:3, labels=c('Failure', 'Non-Existent', 'Success'))
  } else {
    plot(as.numeric(sale_attempts[, var_list[i]]), resid(final_model, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = var_names[i], cex.lab = 1.5
         , cex.axis = 1.5, pch = 16, col = 'blue', cex = 1, ylim = c(-2,2))
  }
  
}

#calculation of the sum of squared residuals to see if they are close to n-p
sum(resid(final_model)^2) # <> n - p, it is not satisfied
