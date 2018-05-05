# Midterm 2.  
# Psych 205. Fall 18.

# -----------------------
# Name: YOUR NAME HERE

# -----------------------

# The midterm is open everything: book, internet, tutorials, etc.
# BUT you need to work on your own.
# Upon completion of the exam, please upload your script to bCourses.
# The test is long - I expect you to cut and paste from the relevant tutorials
# 

# Load the car and Psych library
library(car) 
library(psych)
require(ridge)

# EXERCISE 1
# ------------------------------------------------------------------------------
# We are now going to look at the msq dataset: the data from the Motivational State Questionnaire.
?msq # For additional information on the msq
View(msq)  # View this data set

# We remove the rows with NaN for data that we will use in this excercise 
# For both Problems 1 and 2. You will be using  My.msq as your data.
sel <- complete.cases(msq[,'idle']) & complete.cases(msq[,'inspired']) & complete.cases(msq[, 'intense']) &
       complete.cases(msq[,'interested']) & complete.cases(msq[,'irritable']) & 
       complete.cases(msq[,'satisfied']) & complete.cases(msq[,'scared']) & complete.cases(msq[,'sleepy']) &
       complete.cases(msq[,'strong']) & complete.cases(msq[,'sociable']) & complete.cases(msq[,'happy'])
msq <- msq[sel, ]

# Exercise 1. (0.5 point) Using multiple linear regression, fit a general linear
# model to predict happy using all affects that start 
# with i : idle + inspired + intense + interested + irritable 
# Do not include interaction terms.  
# Briefly state the results.


mod1 = lm(happy ~ idle + inspired + intense + interested + irritable, data=msq)
summary(mod1)
#oefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.83185    0.03739  22.245  < 2e-16 ***
 # idle        -0.02242    0.01829  -1.226  0.22050    
#inspired     0.33816    0.02425  13.943  < 2e-16 ***
#  intense      0.06855    0.02133   3.214  0.00133 ** 
#  interested   0.30902    0.02213  13.962  < 2e-16 ***
#  irritable   -0.31305    0.02184 -14.334  < 2e-16 ***
#  ---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.7347 on 2017 degrees of freedom
#(#1873 observations deleted due to missingness)
#Multiple R-squared:  0.3964,	Adjusted R-squared:  0.3949 
#F-statistic: 264.9 on 5 and 2017 DF,  p-value: < 2.2e-16


#The R squared and adjusted R2 are not that high, implying not that much of the variability is explained
# All of the terms but idle are significantly related, implying the coefficient for idle may be actually indistuingushable from 0
# irritable decreases happiness, as does idle (tho insignficiant)
#the others increase happiness as they increase


# Exercise 2. (1 point) Calculate the adjusted R^2, F-value, and p-value for 
# your model from 1a "by hand".  To do so use simple R commands to calculate SS1 - also known, in this context,
# as SStotal and SS2, also known, in this context, as SSerror.  
# Compare those values to those obtained in 1. (if they are not the same you are doing something wrong)



mod0 = lm(happy ~ 1, data=msq)
y_hat_full = predict(mod1)
y_hat_null = predict(mod0)


ssfull = sum((msq$happy - y_hat_full)^2)
ss0 = sum((msq$happy - y_hat_null)^2)

R2 = 1 - (ssfull/ss0)
print(R2)
summary(mod1)$r.square

n = length(mod1$model$happy)
k = length(mod1$coefficients)

R2adj = 1 - (ssfull/(n-k))/(ss0/(n-1))
print(R2adj)
summary(mod1)$adj.r.square

F_val = ((ss0 - ssfull)/(k-1))/((ssfull)/(n-k))
print(F_val)

require(stats)
#k = 6
pfval = 1 - pf(F_val, df1 = (6-1), df2 = (n-6))
print(pfval)
summary(mod1)
# F-statistic: 263.1 on 5 and 2004 DF,  p-value: < 2.2e-16

# ALL LOOK CORRECT


# Exercise 3. (0.5 point)
# You will now fit two additional models. 
# In the second model, you will 
# predict predict happiness as a function of:
#   satisfied, scared, sleepy, strong and sociable.
# In the third model, you will combine all these predictors and use:
#  idle, inspired, intense, interested, irritable, satisfied, scared, sleepy, strong and sociable.

mod2 = lm(happy ~ satisfied + scared + sleepy + strong + sociable, data=msq)

mod3 = lm(happy ~ idle + inspired + intense + interested + irritable + satisfied + scared + sleepy + strong + sociable, data=msq)



# Exercise 4. (1 point)
# Using classical approaches perform the pair-wise model comparison(s) for all
# three models fitted above that can
# be performed and report which one(s) cannot and why. 
compare13 = anova(mod3, mod1)
compare23 = anova(mod3, mod2)
(compare13)
(compare23)
#we cannot do a comparison between model 1 and model 2 because they are not nested models
#they use totally different sets of values

#The results for both comparisons we can do suggest that the i and s model outperforms
# the i or s models alone, based on significant p values for their f stats

#You can see below, this comparison doen't make sense -- it gives very bizarre results
#so, we cannot do it as not nested (see above)
anova(mod1, mod2)

# Exercise 5. (4 points)
# Using the same approach as in tutorial6 perform a 20 fold cross-validation
# to obtain the mean of square errors (MSE) for each of the three models and differences in MSE that you can
# use to compare models.
# Hints: 
#       1. Copy and paste the relevant code from tutorial6
#       2. All the MSE can be calculated in one loop. (take the mean() of the error)
#       2. No need to calculate R2 (you could if you wanted it but it is equivalent)
#       3. Store the values of MSE for each model in separate array variables.
#       4. Calculate the difference in MSE (for the 3 pair-wise comparisons) for EACH cv-fold as these errors might be correlated.
#       5. You can calculate and store these differences inside the loop or perform a vector arithmetic outside the loop.
#          
# Once you are done with the calculations in the for loop, 
# use the mean and the standard error of the differences in MSE 
# to draw some conclusions about the differences in model predictions. 
# Hints: Here std = se and you can assume that mean+-2.se correspond to 95% confidence intervals.

# Thic code to help you...
n.folds <- 20
folds <- cut(seq(1,nrow(msq)),breaks=n.folds,labels=FALSE)

# Make space for your arrays

# Space for MSE
MSE.imod <- array(data=0, dim = n.folds)
MSE.smod <- array(data=0, dim = n.folds)
MSE.ismod <- array(data=0, dim = n.folds)

#second table for backup calculations
MSE1 <- array(data=0, dim = n.folds)
MSE2 <- array(data=0, dim = n.folds)
MSE3 <- array(data=0, dim = n.folds)


# Space for differences in MSE
DE.i.is <- array(data=0, dim = n.folds)
DE.s.is <- array(data=0, dim = n.folds)
DE.i.s <- array(data=0, dim = n.folds)

#create a second table as a backup
DE.i.is2 <- array(data=0, dim = n.folds)
DE.s.is2 <- array(data=0, dim = n.folds)
DE.i.s2 <- array(data=0, dim = n.folds)

#Perform n.folds fold cross validation
# YOUR CODE and CONCLUSIONS HERE 

for(i in 1:n.folds){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- msq[testIndexes, ]
  trainData <- msq[-testIndexes, ]
  
  # The three models
  mod1_cv = lm(happy ~ idle + inspired + intense + interested + irritable, data=trainData)
  mod2_cv = lm(happy ~ satisfied + scared + sleepy + strong + sociable, data=trainData)
  mod3_cv = lm(happy ~ idle + inspired + intense + interested + irritable + satisfied + scared + sleepy + strong + sociable, data=trainData)
  
  # Get predictions
  yhat_mod1_cv <- predict(mod1_cv, newdata = testData)
  yhat_mod2_cv <- predict(mod2_cv, newdata = testData)
  yhat_mod3_cv <- predict(mod3_cv, newdata = testData)
  mean_cv <- mean(testData$happy)
  
  # Calculate MSE
  MSE_mod1 <- mean(((testData$happy - yhat_mod1_cv)^2))
  MSE_mod2 <- mean(((testData$happy - yhat_mod2_cv)^2))
  MSE_mod3 <- mean(((testData$happy - yhat_mod3_cv)^2))
  
  #this finds MSEs
  MSE.imod[i] <- MSE_mod1
  MSE.smod[i] <- MSE_mod2
  MSE.ismod[i] <- MSE_mod3
  
  #this finds MSE diffs
  DE.i.is[i] = MSE_mod1 - MSE_mod3
  DE.s.is[i] = MSE_mod2 - MSE_mod3
  DE.i.s[i] = MSE_mod1 - MSE_mod2
  
  #Alternate MSE calculation
  MSE1cv = mean(mod1_cv$residuals^2)
  MSE2cv = mean(mod2_cv$residuals^2)
  MSE3cv = mean(mod3_cv$residuals^2)
  
  MSE1[i] = MSE1cv
  MSE2[i] = MSE2cv
  MSE3[i] = MSE3cv
  
  DE.i.is2[i] = MSE1cv - MSE3cv
  DE.s.is2[i] = MSE2cv - MSE3cv
  DE.i.s2[i] = MSE1cv - MSE2cv
  
}

#confirm by hand calcs
mean(MSE1)
mean(MSE.imod)
#.53
mean(MSE2)
mean(MSE.smod)
#.43
mean(MSE3)
mean(MSE.ismod)
#.39

#Without looking at standard errors, it looks like i mod < s mod < is mod in terms of fit

# Once you are done with the calculations in the for loop, 
# use the mean and the standard error of the differences in MSE 
# to draw some conclusions about the differences in model predictions. 
# Hints: Here std = se and you can assume that mean+-2.se correspond to 95% confidence intervals.

sd1 = sd(MSE1)
sd2 = sd(MSE2)
sd3 = sd(MSE3)

mean(MSE1) - sd1*2
#.52
mean(MSE2) - sd2*2
#.4277
mean(MSE3) + sd3*2
#.398

#the 95% confidence interval of MSE for is model doesn't overlap the 95% interval for
# either of the other two models, so we can say it is indeed better

mean(MSE1) - sd1*2
#.52
mean(MSE2) + sd2*2
#.44

#the 95% confidence interval of MSE for the s model doesn't overlap the 95% interval for the 
# i model either, so it does look better

#We can confirm this by graphing the models (i > s > is from left to right)
MSEs = c(mean(MSE1), mean(MSE2), mean(MSE3))
sdMSEs = c(sd1, sd2, sd3)
require(Hmisc)
plot(c(1,2,3), MSEs, type="l", ylim=c(0,.8)) #plot parameters vs R2cv
errbar(c(1,2,3), MSEs, MSEs+sdMSEs, MSEs-sdMSEs, add=T, pch=1, cap=.1) 
#Definitely no overlap in confidence intervals


# Exercise 6. (3 points)
# Cut and copy the code from exercise 5 and modify it to compare cross-validated MSE for 
# the i and s model estimated with lm() versus estimated with linearRidge() or lm.ridge().
# Does ridge prevent overfitting?
library(ridge)         

n.folds <- 20
folds <- cut(seq(1,nrow(msq)),breaks=n.folds,labels=FALSE)

# Make space for your arrays
MSE.ismod.lm <- array(data=0, dim = n.folds)
MSE.ismod.ridge <- array(data=0, dim = n.folds)

#Perform n.folds fold cross validation
# YOUR CODE and Conclusions here
i=1
for(i in 1:n.folds){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- msq[testIndexes, ]
  trainData <- msq[-testIndexes, ]
  
  # The old i s model
  ismod_cv = lm(happy ~ idle + inspired + intense + interested + irritable + satisfied + scared + sleepy + strong + sociable, data=trainData)
  
  
  # Get predictions
  yhat_mod1_cv <- predict(ismod_cv, newdata = testData)
  
  # Calculate MSE
  MSE_lm = mean(((testData$happy - yhat_mod1_cv)^2))
  
  #save to array
  MSE.ismod.lm[i] <- MSE_lm
  
  #The ridge model
  ridgemod <- linearRidge(happy ~ idle + inspired + intense + interested + irritable + satisfied + scared + sleepy + strong + sociable,data=trainData)
  ridgecoefs <- coef(ridgemod)
  mM <- model.matrix(happy ~ idle + inspired + intense + interested + irritable + satisfied + scared + sleepy + strong + sociable,data=testData)
  
  Yhats <- ridgecoefs%*%t(mM)
  
  resids <-  testData$happy - Yhats
  
  MSE_ridge <- mean((resids^2))
  
  #save MSE for ridge model
  MSE.ismod.ridge[i] <- MSE_ridge
  
}

mean(MSE.ismod.lm)
mean(MSE.ismod.ridge)

mean(MSE.ismod.lm) - 2*sd(MSE.ismod.lm)
mean(MSE.ismod.ridge) + 2*sd(MSE.ismod.ridge)

#The ridge model does not appear to prevent overfitting significantly, as MSE are very similar
#and the 95% conf intervals overlap


MSEs = c(mean(MSE.ismod.lm), mean(MSE.ismod.ridge))
sdMSEs = c(sd(MSE.ismod.lm), sd(MSE.ismod.ridge))
require(Hmisc)
plot(c(1,2), MSEs, type="l", ylim=c(0,.8)) #plot parameters vs R2cv
errbar(c(1,2), MSEs, MSEs+sdMSEs, MSEs-sdMSEs, add=T, pch=1, cap=.1) 
#Definitely almost total overlap in confidence intervals


