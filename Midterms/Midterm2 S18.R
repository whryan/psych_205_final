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
My.msq <- msq[sel, ]

# Exercise 1. (0.5 point) Using multiple linear regression, fit a general linear
# model to predict happy using all affects that start 
# with i : idle + inspired + intense + interested + irritable 
# Do not include interaction terms.  
# Briefly state the results.





# Exercise 2. (1 point) Calculate the adjusted R^2, F-value, and p-value for 
# your model from 1a "by hand".  To do so use simple R commands to calculate SS1 - also known, in this context,
# as SStotal and SS2, also known, in this context, as SSerror.  
# Compare those values to those obtained in 1. (if they are not the same you are doing something wrong)




# Exercise 3. (0.5 point)
# You will now fit two additional models. 
# In the second model, you will 
# predict predict happiness as a function of:
#   satisfied, scared, sleepy, strong and sociable.
# In the third model, you will combine all these predictors and use:
#  idle, inspired, intense, interested, irritable, satisfied, scared, sleepy, strong and sociable.





# Exercise 4. (1 point)
# Using classical approaches perform the pair-wise model comparison(s) for all
# three models fitted above that can
# be performed and report which one(s) cannot and why. 





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
folds <- cut(seq(1,nrow(My.msq)),breaks=n.folds,labels=FALSE)

# Make space for your arrays

# Space for MSE
MSE.imod <- array(data=0, dim = n.folds)
MSE.smod <- array(data=0, dim = n.folds)
MSE.ismod <- array(data=0, dim = n.folds)

# Space for differences in MSE
DE.i.is <- array(data=0, dim = n.folds)
DE.s.is <- array(data=0, dim = n.folds)
DE.i.s <- array(data=0, dim = n.folds)

#Perform n.folds fold cross validation
# YOUR CODE and CONCLUSIONS HERE 


# Exercise 6. (3 points)
# Cut and copy the code from exercise 5 and modify it to compare cross-validated MSE for 
# the i and s model estimated with lm() versus estimated with linearRidge() or lm.ridge().
# Does ridge prevent overfitting?
library(ridge)         

n.folds <- 20
folds <- cut(seq(1,nrow(My.msq)),breaks=n.folds,labels=FALSE)

# Make space for your arrays
MSE.ismod.lm <- array(data=0, dim = n.folds)
MSE.ismod.ridge <- array(data=0, dim = n.folds)

#Perform n.folds fold cross validation
# YOUR CODE and Conclusions here